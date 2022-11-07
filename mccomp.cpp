#include "src/astnodes.hpp"
#include "src/helpers.hpp"
#include "llvm/ADT/APFloat.h"
#include "llvm/ADT/Optional.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Verifier.h"
#include "llvm/MC/TargetRegistry.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/Host.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Target/TargetOptions.h"
#include <algorithm>
#include <cassert>
#include <cctype>
#include <cstdio>
#include <cstdlib>
#include <iostream>
#include <list>
#include <map>
#include <memory>
#include <optional>
#include <queue>
#include <regex>
#include <sstream>
#include <string.h>
#include <string>
#include <system_error>
#include <utility>
#include <vector>

using namespace llvm;
using namespace llvm::sys;

FILE* pFile;

//===----------------------------------------------------------------------===//
// Misc classes
//===----------------------------------------------------------------------===//

//===----------------------------------------------------------------------===//
// Lexer
//===----------------------------------------------------------------------===//

static std::string IdentifierStr; // Filled in if IDENT
static int IntVal;                // Filled in if INT_LIT
static bool BoolVal;              // Filled in if BOOL_LIT
static float FloatVal;            // Filled in if FLOAT_LIT
static std::string StringVal;     // Filled in if String Literal
static int lineNo, columnNo;

static TOKEN returnTok(std::string lexVal, int tok_type) {
    TOKEN return_tok;
    return_tok.lexeme   = lexVal;
    return_tok.type     = tok_type;
    return_tok.lineNo   = lineNo;
    return_tok.columnNo = columnNo - lexVal.length() - 1;
    return return_tok;
}

// Read file line by line -- or look for \n and if found add 1 to line number
// and reset column number to 0
/// gettok - Return the next token from standard input.
static TOKEN gettok() {

    static int LastChar = ' ';
    static int NextChar = ' ';

    // Skip any whitespace.
    while (isspace(LastChar)) {
        if (LastChar == '\n' || LastChar == '\r') {
            lineNo++;
            columnNo = 1;
        }
        LastChar = getc(pFile);
        columnNo++;
    }

    if (isalpha(LastChar) ||
        (LastChar == '_')) { // identifier: [a-zA-Z_][a-zA-Z_0-9]*
        IdentifierStr = LastChar;
        columnNo++;

        while (isalnum((LastChar = getc(pFile))) || (LastChar == '_')) {
            IdentifierStr += LastChar;
            columnNo++;
        }

        if (IdentifierStr == "int")
            return returnTok("int", INT_TOK);
        if (IdentifierStr == "bool")
            return returnTok("bool", BOOL_TOK);
        if (IdentifierStr == "float")
            return returnTok("float", FLOAT_TOK);
        if (IdentifierStr == "void")
            return returnTok("void", VOID_TOK);
        if (IdentifierStr == "bool")
            return returnTok("bool", BOOL_TOK);
        if (IdentifierStr == "extern")
            return returnTok("extern", EXTERN);
        if (IdentifierStr == "if")
            return returnTok("if", IF);
        if (IdentifierStr == "else")
            return returnTok("else", ELSE);
        if (IdentifierStr == "while")
            return returnTok("while", WHILE);
        if (IdentifierStr == "return")
            return returnTok("return", RETURN);
        if (IdentifierStr == "true") {
            BoolVal = true;
            return returnTok("true", BOOL_LIT);
        }
        if (IdentifierStr == "false") {
            BoolVal = false;
            return returnTok("false", BOOL_LIT);
        }

        return returnTok(IdentifierStr.c_str(), IDENT);
    }

    if (LastChar == '=') {
        NextChar = getc(pFile);
        if (NextChar == '=') { // EQ: ==
            LastChar = getc(pFile);
            columnNo += 2;
            return returnTok("==", EQ);
        } else {
            LastChar = NextChar;
            columnNo++;
            return returnTok("=", ASSIGN);
        }
    }

    if (LastChar == '{') {
        LastChar = getc(pFile);
        columnNo++;
        return returnTok("{", LBRA);
    }
    if (LastChar == '}') {
        LastChar = getc(pFile);
        columnNo++;
        return returnTok("}", RBRA);
    }
    if (LastChar == '(') {
        LastChar = getc(pFile);
        columnNo++;
        return returnTok("(", LPAR);
    }
    if (LastChar == ')') {
        LastChar = getc(pFile);
        columnNo++;
        return returnTok(")", RPAR);
    }
    if (LastChar == ';') {
        LastChar = getc(pFile);
        columnNo++;
        return returnTok(";", SC);
    }
    if (LastChar == ',') {
        LastChar = getc(pFile);
        columnNo++;
        return returnTok(",", COMMA);
    }

    if (isdigit(LastChar) || LastChar == '.') { // Number: [0-9]+.
        std::string NumStr;

        if (LastChar == '.') { // Floatingpoint Number: .[0-9]+
            do {
                NumStr += LastChar;
                LastChar = getc(pFile);
                columnNo++;
            } while (isdigit(LastChar));

            FloatVal = strtof(NumStr.c_str(), nullptr);
            return returnTok(NumStr, FLOAT_LIT);
        } else {
            do { // Start of Number: [0-9]+
                NumStr += LastChar;
                LastChar = getc(pFile);
                columnNo++;
            } while (isdigit(LastChar));

            if (LastChar == '.') { // Floatingpoint Number: [0-9]+.[0-9]+)
                do {
                    NumStr += LastChar;
                    LastChar = getc(pFile);
                    columnNo++;
                } while (isdigit(LastChar));

                FloatVal = strtof(NumStr.c_str(), nullptr);
                return returnTok(NumStr, FLOAT_LIT);
            } else { // Integer : [0-9]+
                IntVal = strtod(NumStr.c_str(), nullptr);
                return returnTok(NumStr, INT_LIT);
            }
        }
    }

    if (LastChar == '&') {
        NextChar = getc(pFile);
        if (NextChar == '&') { // AND: &&
            LastChar = getc(pFile);
            columnNo += 2;
            return returnTok("&&", AND);
        } else {
            LastChar = NextChar;
            columnNo++;
            return returnTok("&", int('&'));
        }
    }

    if (LastChar == '|') {
        NextChar = getc(pFile);
        if (NextChar == '|') { // OR: ||
            LastChar = getc(pFile);
            columnNo += 2;
            return returnTok("||", OR);
        } else {
            LastChar = NextChar;
            columnNo++;
            return returnTok("|", int('|'));
        }
    }

    if (LastChar == '!') {
        NextChar = getc(pFile);
        if (NextChar == '=') { // NE: !=
            LastChar = getc(pFile);
            columnNo += 2;
            return returnTok("!=", NE);
        } else {
            LastChar = NextChar;
            columnNo++;
            return returnTok("!", NOT);
            ;
        }
    }

    if (LastChar == '<') {
        NextChar = getc(pFile);
        if (NextChar == '=') { // LE: <=
            LastChar = getc(pFile);
            columnNo += 2;
            return returnTok("<=", LE);
        } else {
            LastChar = NextChar;
            columnNo++;
            return returnTok("<", LT);
        }
    }

    if (LastChar == '>') {
        NextChar = getc(pFile);
        if (NextChar == '=') { // GE: >=
            LastChar = getc(pFile);
            columnNo += 2;
            return returnTok(">=", GE);
        } else {
            LastChar = NextChar;
            columnNo++;
            return returnTok(">", GT);
        }
    }

    if (LastChar ==
        '/') { // could be division or could be the start of a comment
        LastChar = getc(pFile);
        columnNo++;
        if (LastChar == '/') { // definitely a comment
            do {
                LastChar = getc(pFile);
                columnNo++;
            } while (LastChar != EOF && LastChar != '\n' && LastChar != '\r');

            if (LastChar != EOF)
                return gettok();
        } else
            return returnTok("/", DIV);
    }

    // Check for end of file.  Don't eat the EOF.
    if (LastChar == EOF) {
        columnNo++;
        return returnTok("0", EOF_TOK);
    }

    // Otherwise, just return the character as its ascii value.
    int ThisChar = LastChar;
    std::string s(1, ThisChar);
    LastChar = getc(pFile);
    columnNo++;
    return returnTok(s, int(ThisChar));
}

//===----------------------------------------------------------------------===//
// Parser
//===---------------------------------------------------------------------
//-===//

/// CurTok/getNextToken - Provide a simple token buffer.  CurTok is the current
/// token the parser is looking at.  getNextToken reads another token from the
/// lexer and updates CurTok with its results.
static TOKEN CurTok;
static std::deque<TOKEN> tok_buffer;

static TOKEN getNextToken() {

    if (tok_buffer.size() == 0)
        tok_buffer.push_back(gettok());

    TOKEN temp = tok_buffer.front();
    tok_buffer.pop_front();

    return CurTok = temp;
}

static void putBackToken(TOKEN tok) {
    tok_buffer.push_front(tok);
}

//===----------------------------------------------------------------------===//
// AST nodes (in header)
//===----------------------------------------------------------------------===//

//===----------------------------------------------------------------------===//
// Recursive Descent Parser - Function call for each production
//===----------------------------------------------------------------------===//

/* Add function calls for each production */
// TODO add missing declarations

static void misc::restoreState(const TOKEN& prev) {
    putBackToken(CurTok);
    putBackToken(prev);
    getNextToken();
}

UPtrASTnode parse::LogError(const char* str) {
    fprintf(stderr, "Error: %s\n", str);
    return nullptr;
}

template <typename T> std::optional<T> parse::LogErrorOpt(const char* str) {
    fprintf(stderr, "Error: %s\n", str);
    return std::nullopt;
}

/// parse INT_LIT
static UPtrASTnode parse::ParseIntLit() {
    if (CurTok.type != TOKEN_TYPE::INT_LIT)
        return parse::LogError("Expected integer literal");
    auto int_val = std::stoi(CurTok.lexeme);
    auto Ret     = std::make_unique<IntASTnode>(CurTok, IntVal);
    getNextToken(); // consume input
    return std::move(Ret);
}

/// parse Float_LIT
static UPtrASTnode parse::ParseFloatLit() {
    if (CurTok.type != TOKEN_TYPE::FLOAT_LIT)
        return parse::LogError("Expected boolean literal");
    auto float_val = std::stof(CurTok.lexeme);
    auto Ret       = std::make_unique<FloatASTnode>(CurTok, FloatVal);
    getNextToken();
    return std::move(Ret);
}

/// parse Bool_LIT
static UPtrASTnode parse::ParseBoolLit() {
    if (CurTok.type != TOKEN_TYPE::BOOL_LIT)
        return parse::LogError("Expected boolean literal");
    bool bool_val = (CurTok.lexeme == "true") ? true : false;
    auto Ret      = std::make_unique<BoolASTnode>(CurTok, bool_val);
    getNextToken();
    return std::move(Ret);
}

/// parse IDENT
static UPtrASTnode parse::ParseIdent() {
    if (!misc::checkIdent(CurTok))
        return parse::LogError("Expected identifier");
    auto Ret =
        std::make_unique<IdentifierASTnode>(CurTok, std::move(CurTok.lexeme));
    getNextToken(); // Consume token
    return std::move(Ret);
}

/// @brief parse Args
/// Args ::= arg_list First(Arg_list) =
///       | epsilon
/// args_list ::= expr arg_list'
///           | "," expr arg_list'
/// @return Argument list
static std::optional<VectorAST> parse::ParseArgs() { // TODO Fix
    VectorAST args;
    if (misc::checkTokenInGroup(CurTok.type)) {
        if (CurTok.type != TOKEN_TYPE::RPAR) {
            auto Ret = parse::ParseExpr();
            if (!Ret)
                return std::nullopt; // Error in Expr
            args.push_back(std::move(Ret));
        }
        while (CurTok.type != TOKEN_TYPE::RPAR) {
            if (CurTok.type != TOKEN_TYPE::COMMA)
                return LogErrorOpt<VectorAST>("No comma found");
            getNextToken(); // Parse ,
            auto Ret = parse::ParseExpr();
            if (!Ret)
                return std::nullopt; // Error in Expr
            args.push_back(std::move(Ret));
        }
    } else if (CurTok.type == TOKEN_TYPE::INVALID) {
        return parse::LogErrorOpt<VectorAST>("Invalid Character");
    }
    // epsilon
    return std::optional(std::move(args));
}

/// parse IDENT(Args)
/// Assuming previously checked with LL(2)
static UPtrASTnode parse::ParseFunctionCall() {
    auto cTok  = CurTok;
    auto Ident = cTok.lexeme;
    // Assuming Ident ( was checked by previous function
    getNextToken(); // Eat Identifier
    getNextToken(); // Eat '('
    auto Args = parse::ParseArgs();
    if (!Args.has_value())
        return nullptr; // if an error was triggered
    auto Ret = std::make_unique<FunctionCallASTnode>(std::move(Ident),
                                                     std::move(Args.value()));

    if (CurTok.type != TOKEN_TYPE::RPAR)
        return parse::LogError("No Right Parenthesis found");
    return Ret;
}

// expr ::= IDENT = expr
//         | rval
static UPtrASTnode parse::ParseExpr() {
    if (CurTok.type == TOKEN_TYPE::IDENT) {
        auto PrevTok = CurTok;
        getNextToken();
        if (CurTok.type == TOKEN_TYPE::ASSIGN) { // Assign
            getNextToken();                      // Consume =
            auto Expr = parse::ParseExpr();
            if (!Expr)
                return nullptr;
            auto Ret = std::make_unique<AssignmentASTnode>(
                std::move(PrevTok.lexeme), std::move(Expr));
            return std::move(Ret);
        } // if not it means not rval
        misc::restoreState(PrevTok);
    }
    // RVAL eval
    return parse::ParseRVal();
}

/// rval ::= rand rval'
/// rval' ::= || rand rval'
static UPtrASTnode parse::ParseRVal() {
    UPtrASTnode curr = parse::ParseRAnd();
    if (!curr)
        return nullptr;
    while (CurTok.type == TOKEN_TYPE::OR) {
        getNextToken(); // Consume ||
        auto RHS = parse::ParseRAnd();
        if (!RHS)
            return nullptr;
        curr = std::make_unique<BinaryOperatorASTnode>(
            TOKEN_TYPE::OR, std::move(curr), std::move(RHS));
    }
    return std::move(curr);
}

/// rand ::= req rand'
/// rand' ::= && req rand'
static UPtrASTnode parse::ParseRAnd() {
    auto curr = parse::ParseREq();
    if (!curr)
        return nullptr;
    while (CurTok.type == TOKEN_TYPE::AND) {
        getNextToken(); // consume &&
        auto RHS = parse::ParseREq();
        if (!RHS)
            return nullptr;
        curr = std::make_unique<BinaryOperatorASTnode>(
            TOKEN_TYPE::AND, std::move(curr), std::move(RHS));
    }
    return std::move(curr);
}

/// req ::= rineq req'
/// req' ::= (== | !=) rineq req'
static UPtrASTnode parse::ParseREq() {
    auto curr = parse::ParseRIneq();
    if (!curr)
        return nullptr;
    while (CurTok.type == TOKEN_TYPE::NE || CurTok.type == TOKEN_TYPE::EQ) {
        auto PrevToken = CurTok;
        getNextToken(); // Consume Symb
        auto RHS = parse::ParseRIneq();
        if (!RHS)
            return nullptr;
        curr = std::make_unique<BinaryOperatorASTnode>(
            static_cast<TOKEN_TYPE>(PrevToken.type), std::move(curr),
            std::move(RHS));
    }
    return std::move(curr);
}

/// rineq ::= rpm rineq'
/// rineq' ::= ( <= | < | > | >=) pm rval'
static UPtrASTnode parse::ParseRIneq() {
    auto curr = parse::ParseRPm();
    if (!curr)
        return nullptr;

    while (misc::checkTokInGroup(CurTok, std::vector<TOKEN_TYPE>{
                                             TOKEN_TYPE::LE,
                                             TOKEN_TYPE::LT,
                                             TOKEN_TYPE::GE,
                                             TOKEN_TYPE::GT,
                                         })) {
        auto PrevToken = CurTok;
        getNextToken(); // Consume Binary operator
        auto RHS = parse::ParseRPm();
        if (!RHS)
            return nullptr;
        curr = std::make_unique<BinaryOperatorASTnode>(
            static_cast<TOKEN_TYPE>(PrevToken.type), std::move(curr),
            std::move(RHS));
    }

    return std::move(curr);
}

/// rpm ::= rmdm rpm'
/// rpm' ::= (+ | -) rmdm rval'
static UPtrASTnode parse::ParseRPm() {
    auto curr = parse::ParseRMdm();
    if (!curr)
        return nullptr;
    while (CurTok.type == TOKEN_TYPE::PLUS ||
           CurTok.type == TOKEN_TYPE::MINUS) {
        auto PrevToken = CurTok;
        getNextToken(); // Absorb + or -
        auto RHS = parse::ParseRMdm();
        if (!RHS)
            return nullptr;
        curr = std::make_unique<BinaryOperatorASTnode>(
            static_cast<TOKEN_TYPE>(PrevToken.type), std::move(curr),
            std::move(RHS));
    }
    return std::move(curr);
}

/// rmdm ::= rp rmdm'
/// rmdm' ::= (* | / | %) rp rmdm'
static UPtrASTnode parse::ParseRMdm() {
    auto curr = parse::ParseRp();
    if (!curr)
        return nullptr;
    if (CurTok.type == TOKEN_TYPE::DIV || CurTok.type == TOKEN_TYPE::ASTERIX ||
        CurTok.type == TOKEN_TYPE::MOD) {
        auto PrevToken = CurTok;
        getNextToken(); // Absorb symbol
        auto RHS = parse::ParseRp();
        if (!RHS)
            return nullptr;
        curr = std::make_unique<BinaryOperatorASTnode>(
            static_cast<TOKEN_TYPE>(PrevToken.type), std::move(curr),
            std::move(RHS));
        // getNextToken(); // TODO check if needed
        return std::move(curr);
    }
    // getNextToken(); // TODO check if needed
    return std::move(curr);
}

/// rp ::= (expr)
///      | - expr
///      | ! expr
///      | IDENT
///      | IDENT (expr)
///      | INT_LIT
///      | FLOAT_LIT
///      | BOOL_LIT
static UPtrASTnode parse::ParseRp() {
    if (CurTok.type == TOKEN_TYPE::LPAR) { // (expr)
        getNextToken();                    // Consume LPar
        auto InnerExpr = parse::ParseExpr();
        if (!InnerExpr)
            return nullptr;
        if (CurTok.type != TOKEN_TYPE::RPAR)
            return parse::LogError("Expected RPar");
        getNextToken(); // Consume RPar
        return std::move(InnerExpr);
    } else if (CurTok.type == IDENT) { // check for IDENT
        auto PrevTok = CurTok;
        getNextToken();
        if (CurTok.type ==
            TOKEN_TYPE::LPAR) { // check for IDENT ( expr ) (LL(2))
            misc::restoreState(PrevTok);
            auto FuncCall = parse::ParseFunctionCall();
            if (!FuncCall)
                return nullptr;
            if (CurTok.type != TOKEN_TYPE::RPAR)
                return parse::LogError("Expected Rpar");
            getNextToken();
            return std::move(FuncCall);
        }
        misc::restoreState(PrevTok);
        auto Ident = parse::ParseIdent();
        if (!Ident)
            return nullptr;
        return std::move(Ident);
    } else if (CurTok.type == TOKEN_TYPE::MINUS ||
               CurTok.type == TOKEN_TYPE::NOT) { // - expr | ! expr
        auto PrevTok = CurTok;
        getNextToken(); // Consume unary
        auto Expr = parse::ParseExpr();
        if (!Expr)
            return nullptr;
        auto UnaryNode = std::make_unique<UnaryOperatorASTnode>(
            static_cast<TOKEN_TYPE>(PrevTok.type), std::move(Expr));
        return std::move(UnaryNode);
    }
    UPtrASTnode Ret = nullptr;
    switch (CurTok.type) {
    case TOKEN_TYPE::BOOL_LIT:
        Ret = parse::ParseBoolLit();
        break;
    case TOKEN_TYPE::INT_LIT:
        Ret = parse::ParseIntLit();
        break;
    case TOKEN_TYPE::FLOAT_LIT:
        Ret = parse::ParseFloatLit();
        break;
    }
    if (!Ret)
        return parse::LogError("No expression token found");
    return std::move(Ret);
}

static parse::StmtType parse::ParseStmt() {
    if (CurTok.type == TOKEN_TYPE::SC) {
        getNextToken();              // Absorb SC
        return parse::StmtType('c'); // Semi
    }
    if (misc::checkTokenInGroup(CurTok.type)) { // Check for expr_stmt
        auto Expr = parse::ParseExpr();
        if (!Expr)
            return parse::StmtType(false); // If the expr parsing failed
        if (CurTok.type != TOKEN_TYPE::SC) {
            parse::LogErrorOpt<UPtrASTnode>("No semicolon was found");
            return parse::StmtType(false);
        }
        getNextToken(); // Absorb semi colon
        return parse::StmtType(std::move(Expr));
    } else if (CurTok.type == TOKEN_TYPE::IF) {
        return parse::ParseIf();
    } else if (CurTok.type == TOKEN_TYPE::RETURN) {
        return parse::ParseReturn();
    } else if (CurTok.type == TOKEN_TYPE::WHILE) {
        return parse::ParseWhile();
    } else if (CurTok.type == TOKEN_TYPE::LBRA) {
        return parse::ParseBlock();
    }
    parse::LogError("No expression or semicolon was found");
    return parse::StmtType(false);
}

static std::optional<VectorAST> parse::ParseStmtList() {
    auto StList = VectorAST();
    while (CurTok.type != TOKEN_TYPE::RBRA) {
        auto Stmt = parse::ParseStmt();
        if (std::holds_alternative<Semi>(Stmt)) // Indicates ;
            continue;
        if (std::holds_alternative<Err>(Stmt)) // Error occured
            return std::nullopt;
        StList.push_back(std::move(std::get<UPtrASTnode>(Stmt)));
    }
    return std::make_optional<VectorAST>(std::move(StList));
}

static UPtrASTnode parse::ParseTypeIdentSC() {
    auto Ret = parse::ParseTypeIdent();
    if (!Ret)
        return nullptr;
    if (CurTok.type != TOKEN_TYPE::SC)
        return parse::LogError("No semicolon found");
    getNextToken(); // Consume SC
    return std::move(Ret);
}

static std::optional<VectorAST> parse::ParseLocalDelcs() {
    auto LocalDecls = VectorAST();
    while (misc::checkTokenVarType(CurTok.type)) {
        auto Decl = parse::ParseTypeIdentSC();
        if (!Decl)
            return std::nullopt;
        LocalDecls.push_back(std::move(Decl));
    }
    return std::make_optional<VectorAST>(std::move(LocalDecls));
}

static UPtrASTnode parse::ParseProgram() {
    auto ExternLis = VectorAST();
    auto StmtLis   = VectorAST();
    while (CurTok.type == TOKEN_TYPE::EXTERN) {
        auto ExternNode = parse::ParseExtern();
        if (!ExternNode)
            return nullptr;
        ExternLis.push_back(std::move(ExternNode));
    }

    while (CurTok.type != TOKEN_TYPE::EOF_TOK) {
        auto DeclNode = parse::ParseDecl();
        if (!DeclNode)
            return nullptr;
        StmtLis.push_back(std::move(DeclNode));
    }
    return std::make_unique<ProgramASTnode>(std::move(ExternLis),
                                            std::move(StmtLis));
}

static UPtrASTnode parse::ParseDecl() {
    auto Var_Type = CurTok;
    if (Var_Type.type == TOKEN_TYPE::VOID_TOK) {
        // Run function call
        return ParseFunction();
    }

    if (!misc::checkTokenVarType(Var_Type.type))
        return parse::LogError("No type found");
    getNextToken(); // Consume Type
    auto Ident = CurTok;
    if (CurTok.type != TOKEN_TYPE::IDENT)
        return parse::LogError("No Identifier found");
    getNextToken();                      // Consume name
    if (CurTok.type == TOKEN_TYPE::SC) { // Var decl
        // parse decl
        getNextToken(); // Consume SC
        return std::make_unique<DeclarationASTnode>(
            std::move(Ident.lexeme), static_cast<TOKEN_TYPE>(Var_Type.type));
    } else if (CurTok.type == TOKEN_TYPE::LPAR) { // Fun decl
        auto Params = parse::ParseParams();       // () Consumed in parseblock
        if (!Params.has_value())
            return nullptr;
        auto block = parse::ParseBlock(); // {} Consumed in parseblock
        if (!block)
            return nullptr;
        return std::make_unique<FunctionASTnode>(
            std::move(Params.value()), std::move(block),
            std::move(Ident.lexeme), static_cast<TOKEN_TYPE>(Var_Type.type));
    }
    return parse::LogError("Invalid declaration");
}

static UPtrASTnode parse::ParseTypeIdent() {
    auto Var_Type = CurTok;
    if (!misc::checkTokenVarType(Var_Type.type))
        return parse::LogError("No 1type found");
    getNextToken(); // Consume Type
    auto Ident = CurTok;
    if (CurTok.type != TOKEN_TYPE::IDENT)
        return parse::LogError("No Identifier found");
    getNextToken(); // Consume name
    return std::make_unique<DeclarationASTnode>(
        std::move(Ident.lexeme), static_cast<TOKEN_TYPE>(Var_Type.type));
}

static std::optional<Args_t> parse::ParseParams() {
    if (CurTok.type != TOKEN_TYPE::LPAR)
        return parse::LogErrorOpt<Args_t>("No LPAR found");
    getNextToken(); // Consume LPar
    Args_t Decl;
    if (CurTok.type == TOKEN_TYPE::VOID_TOK) {
        Decl = 'c';
        getNextToken(); // Consume void
    } else {
        auto Arg_list = VectorAST(); // List of arguments
        if (CurTok.type != TOKEN_TYPE::RPAR) {
            auto Decl = parse::ParseTypeIdent();
            if (!Decl)
                return std::nullopt;
            Arg_list.push_back(std::move(Decl));
        }
        while (CurTok.type != TOKEN_TYPE::RPAR) {
            if (CurTok.type != TOKEN_TYPE::COMMA) {
                return parse::LogErrorOpt<Args_t>("No comma found");
            }
            getNextToken(); // Consume comma
            auto Decl = parse::ParseTypeIdent();
            if (!Decl)
                return std::nullopt;
            Arg_list.push_back(std::move(Decl));
        }
        Decl = std::move(Arg_list);
    }
    getNextToken(); // Consume Rpar
    return std::optional(std::move(Decl));
}

static UPtrASTnode parse::ParseExtern() {
    getNextToken(); // Consume extern
    auto Type = CurTok;
    if (!misc::checkTokenRetType(Type.type))
        return parse::LogError("No type found");
    getNextToken(); // Consume Type
    auto Ident = CurTok;
    if (Ident.type != TOKEN_TYPE::IDENT)
        return parse::LogError("No identifier found");
    getNextToken();                   // Consume Ident
    auto Decl = parse::ParseParams(); // () Consumed in ParseParams
    if (!Decl.has_value())
        return nullptr;
    if (CurTok.type != TOKEN_TYPE::SC)
        return parse::LogError("No SC found");
    getNextToken(); // Consume ;
    return std::make_unique<ExternFunctionDeclASTnode>(
        std::move(Ident.lexeme), static_cast<TOKEN_TYPE>(Type.type),
        std::move(Decl.value()));
}

// Do until local_delcs
static UPtrASTnode parse::ParseBlock() {
    if (CurTok.type != TOKEN_TYPE::LBRA)
        return parse::LogError("No left bracket found");
    getNextToken(); // Consume LBRA
    auto LocalRes = parse::ParseLocalDelcs();
    if (!LocalRes.has_value()) // Error happened
        return nullptr;
    auto LocalD  = std::move(LocalRes.value());
    auto StmtRes = parse::ParseStmtList();
    if (!StmtRes.has_value())
        return nullptr;
    if (CurTok.type != TOKEN_TYPE::RBRA)
        return parse::LogError("No right bracket found");
    getNextToken(); // Consume RBRA
    auto StmtL = std::move(StmtRes.value());
    return std::make_unique<BodyASTnode>(std::move(LocalD), std::move(StmtL));
}

static UPtrASTnode parse::ParseFunction() {
    auto RetType = CurTok;
    if (!misc::checkTokenRetType(RetType.type))
        return parse::LogError("Not return type found");
    getNextToken();
    auto Ident = CurTok;
    if (!misc::checkIdent(Ident))
        return parse::LogError("No identifier found");
    getNextToken();                     // Consume identifier
    auto Params = parse::ParseParams(); // () Consumed in ParseParams
    if (!Params.has_value())
        return nullptr;
    auto block = parse::ParseBlock(); // {} Consumed RBra
    if (!block)
        return nullptr;
    return std::make_unique<FunctionASTnode>(
        std::move(Params.value()), std::move(block), std::move(Ident.lexeme),
        static_cast<TOKEN_TYPE>(RetType.type));
}

static UPtrASTnode parse::ParseIf() {
    if (CurTok.type != TOKEN_TYPE::IF)
        return parse::LogError("Expected If token");
    getNextToken(); // Consume If

    if (CurTok.type != TOKEN_TYPE::LPAR)
        return parse::LogError("Expected ( token");
    getNextToken(); // Consume (
    auto Expr = parse::ParseExpr();
    if (!Expr)
        return nullptr;
    if (CurTok.type != TOKEN_TYPE::RPAR)
        return parse::LogError("Expected ) token");
    // Consume )
    getNextToken();
    auto Block = parse::ParseBlock();
    if (!Block)
        return nullptr;
    auto Else = parse::ParseElse();
    if (Else.has_value() &&
        !Else.value()) { // If an else statement was found but parsing failed
        return nullptr;
    }
    return std::make_unique<IfStatementASTnode>(
        std::move(Expr), std::move(Block), std::move(Else));
}

static OptionalPtr parse::ParseElse() {
    if (CurTok.type == TOKEN_TYPE::ELSE) {
        getNextToken(); // Consume else
        return std::optional(parse::ParseBlock());
    }
    return std::nullopt;
}
//
static UPtrASTnode parse::ParseWhile() {
    if (CurTok.type != TOKEN_TYPE::WHILE)
        return parse::LogError("Expected while token");
    getNextToken(); // consume While
    if (CurTok.type != TOKEN_TYPE::LPAR)
        return parse::LogError("Expected ( token");
    getNextToken(); // consume (
    auto Expr = ParseExpr();
    if (!Expr)
        return nullptr;
    if (CurTok.type != TOKEN_TYPE::RPAR)
        return parse::LogError("Expected ) token");
    getNextToken(); // consume )
    auto Stmt = parse::ParseStmt();
    if (std::holds_alternative<Err>(Stmt))
        return nullptr;
    UPtrASTnode Body = nullptr;
    if (std::holds_alternative<UPtrASTnode>(Stmt))
        Body = std::move(std::get<UPtrASTnode>(Stmt));
    return std::make_unique<WhileStatementASTnode>(std::move(Expr),
                                                   std::move(Body));
}

/// return_stmt ::= "return" expr ";"
///              | "return" ;
static UPtrASTnode parse::ParseReturn() {
    if (CurTok.type != TOKEN_TYPE::RETURN) // Check for return
        return parse::LogError("Expected return token");
    getNextToken(); // Consume return
    std::optional<UPtrASTnode> Expr =
        std::nullopt; // Optional incase for no expression
    if (misc::checkTokenInGroup(CurTok.type)) // Check if exists expression
        Expr = std::optional<UPtrASTnode>(
            parse::ParseExpr()); // Parse expression and put in an optional
    if (CurTok.type != TOKEN_TYPE::SC) // Force to finish with SC
        return parse::LogError("Expected ; token");
    getNextToken(); // Parse ;
    return std::make_unique<ReturnStatementASTnode>(std::move(Expr));
}

//===----------------------------------------------------------------------===//
static void parser() {
    // add body
    auto Ret = parse::ParseProgram();
    if (!Ret) {
        std::cout << "Error" << std::endl;
    } else {
        std::cout << "Parsed Successfully" << std::endl;
        std::cout << Ret->to_string() << std::endl;
    }
}

//===----------------------------------------------------------------------===//
// Code Generation
//===----------------------------------------------------------------------===//

// TODO FINISH CODEGEN (BY END OF WEEK)
// TODO FIX OUTPUT (BY END OF WEEK)
// TODO APPLY LAZY EVAL(BY END OF WEEK)
// TODO WARNING ON TYPE CONV

static LLVMContext
    TheContext; // Opaque object that owns a lot of core llvm data structures
static IRBuilder<> Builder(TheContext); // Helps to generate llvm instructures
static std::unique_ptr<Module>
    TheModule; // contains functions and global variables, owns mem for all data
               // we generate
static std::list<std::map<std::string, AllocaInst*>>
    NamedValues; // Will keep track of wich values are defined in the current
                 // scope Helps to keep track their llvm representation
                 // IE  : SymbolTable

static std::map<std::string, Value*>
    GlobalVariables; // Will keep of global variables

std::optional<Value*> LogErrorV(const char* str) {
    LogError(Str);
    return std::optional<nullptr>;
}

static Value* misc::findElem(const std::string& Name) {
    for (auto it = NamedValues.rbegin(); it != NamedValues.rend();
         it++) { // Start from the most local scope and go backwards
        if (it[Name]) {
            Type* typ;
            if (it[Name]->getType()->isFloatTy()) {
                typ = Type::getFloatTy(TheContext);
            } else {
                typ = Type::getInt32Ty(TheContext);
            }
            return Builder.CreateLoad(typ, it[Name], Name);
        }
    }
    return GlobalVariables[Name];
}

static AllocaInst* misc::CreateEntryBlockAlloca(Function* TheFunction,
                                                const std::string& VarName,
                                                const Type* typ) {
    IRBuilder<> TmpB(&TheFunction->getEntryBlock(),
                     TheFunction->getEntryBlock().begin());
    return TmpB.CreateAlloca(typ, 0, VarName.c_str());
}

std::optional<Value*> FloatASTnode::codegen() {
    return std::optional(ConstantFP::get(TheContext, APFloat(Val)));
}

std::optional<Value*> IntASTnode::codegen() {
    return std::optional(ConstantInt::get(TheContext, APInt(Val)));
}

std::optional<Value*> BoolASTnode::codegen() {
    int val = Val ? 1 : 0;
    return std::optional(ConstantInt::get(TheContext, APInt(val)));
}

std::optional<Value*> IdentifierASTnode::codegen() {
    // Look up variable in function
    Value* V = misc::findElem(Name, typ);
    if (!V) {
        return LogErrorV("Unknown variable");
    }
    return std::optional(V);
}

std::optional<Value*> BinaryOperatorASTnode::codegen() {
    Value* L = LHS->codegen();
    Value* R = RHS->codegen();
    if (!L || !R)
        return std::optional(nullptr);
    if (L->getType()->getTypeID() !=
        R->getType()->getTypeID()) {     // L : Float R : Int or vice versa
        if (L->getType()->isFloatTy()) { // Case 1
            R = Builder.CreateSIToFP(R, Type::getFloatTy(TheContext),
                                     "floattmp");
        } else { // else case
            L = Builder.CreateSIToFP(L, Type::getFloatTy(TheContext),
                                     "floattmp");
        }
    }
    // TODO ASK ABOUT ORDERING
    if (L->getType()->isFloatTy()) // Since both have the same value, check
                                   // if float

        switch (Op) {
        case TOKEN_TYPE::PLUS:
            return std::optional(Builder.CreateFAdd(L, R, "f_addtmp"));
            break;
        case TOKEN_TYPE::MINUS:
            return std::optional(Builder.CreateFSub(L, R, "f_subtmp"));
            break;
        case TOKEN_TYPE::ASTERIX:
            return std::optional(Builder.CreateFMul(L, R, "f_multmp"));
            break;
        case TOKEN_TYPE::DIV:
            return std::optional(Builder.CreateFDiv(L, R, "f_divtmp"));
            break;
        case TOKEN_TYPE::MOD:
            return LogErrorV("Cannot use modulo on floating numbers");
            break;
        case TOKEN_TYPE::EQ:
            return std::optional(Builder.CreateFCmpOEQ(L, R, "f_eqtmp"));
            break;
        case TOKEN_TYPE::NE:
            return std::optional(Builder.CreateFCmpONE(L, R, "f_netmp"));
            break;
        case TOKEN_TYPE::LE:
            return std::optional(Builder.CreateFCmpOLE(L, R, "f_letmp"));
            break;
        case TOKEN_TYPE::LT:
            return std::optional(Builder.CreateFCmpOLT(L, R, "f_lttmp"));
            break;
        case TOKEN_TYPE::GE:
            return std::optional(Builder.CreateFCmpOGE(L, R, "f_getmp"));
            break;
        case TOKEN_TYPE::GT:
            return std::optional(Builder.CreateFCmpOGT(L, R, "f_gttmp"));
            break;
        case TOKEN_TYPE::AND:
            L = Builder.CreateFPToSI(L, Type::getInt32Ty(TheContext),
                                     "li_temp");
            R = Builder.CreateFPToSI(R, Type::getInt32Ty(TheContext),
                                     "ri_temp");
            return std::optional(Builder CreateAnd(L, R, "andtmp"));
        case TOKEN_TYPE::OR:
            L = Builder.CreateFPToSI(L, Type::getInt32Ty(TheContext),
                                     "li_temp");
            R = Builder.CreateFPToSI(R, Type::getInt32Ty(TheContext),
                                     "ri_temp");
            return std::optional(Builder.CreateOr(L, R, "ortmp"));
        }
    else
        switch (Op) {
        case TOKEN_TYPE::PLUS:
            return std::optional(Builder.CreateAdd(L, R, "i_addtmp"));
            break;
        case TOKEN_TYPE::MINUS:
            return std::optional(Builder.CreateSub(L, R, "i_subtmp"));
            break;
        case TOKEN_TYPE::ASTERIX:
            return std::optional(Builder.CreateMul(L, R, "i_multmp"));
            break;
        case TOKEN_TYPE::DIV:
            return std::optional(Builder.CreateSDiv(L, R, "i_divtmp"));
            break;
        case TOKEN_TYPE::MOD: // TODO
            return std::optional(Builder.CreateSRem(L, R, "i_modtmp"));
            break;
        case TOKEN_TYPE::EQ:
            return std::optional(Builder.CreateICmpEQ(L, R, "i_eqtmp"));
            break;
        case TOKEN_TYPE::NE:
            return std::optional(Builder.CreateICmpNE(L, R, "i_netmp"));
            break;
        case TOKEN_TYPE::LE:
            return std::optional(Builder.CreateICmpLE(L, R, "i_letmp"));
            break;
        case TOKEN_TYPE::LT:
            return std::optional(Builder.CreateICmpLT(L, R, "i_lttmp"));
            break;
        case TOKEN_TYPE::GE:
            return std::optional(Builder.CreateICmpGE(L, R, "i_getmp"));
            break;
        case TOKEN_TYPE::GT:
            return std::optional(Builder.CreateICmpGT(L, R, "i_gttmp"));
            break;
        case TOKEN_TYPE::AND:
            return std::optional(Builder.CreateAnd(L, R, "andtmp"));
        case TOKEN_TYPE::OR:
            return std::optional(Builder.CreateOr(L, R, "ortmp"));
        }
    return std::optional(nullptr);
}

std::optional<Value*> UnaryOperatorASTnode::codegen() {
    Value* f = Expr->codegen();
    if (!f)
        return nullptr;
    if (f->getType()->isFloatTy()) {
        if (Op == TOKEN_TYPE::MINUS)
            return std::optional(Builder.CreateFNeg(f, "f_negtmp"));
        f = Builder.CreateFPToSI(f, "int_temp");
    }
    switch (Op) {
    case TOKEN_TYPE::NOT:
        return std::optional(Builder.CreateNot(f, "i_nottmp"));
    case TOKEN_TYPE::MINUS:
        return std::optional(Builder.CreateNeg(f, "i_temp"));
    }
}

std::optional<Value*> DeclarationASTnode::codegen() {
    Type* typ;
    if (Type == TOKEN_TYPE::FLOAT_TOK) {
        typ = Type::getFloatTy(TheContext);
    } else {
        typ = Type::getInt32Ty(TheContext);
    }
    AllocaInst* var = CreateEntryBlockAlloca(
        Builder.GetInsertBlock()->getParent(), Name, typ);
    NamedValues.back().insert({Name, var}); // Add value to the local scope
    return std::nullopt;
}

std::optional<Value*> AssignmentASTnode::codegen() {
    Value* V = misc::findElem(Name);
    if (!V)
        return LogError("Variable named " + Name + " not found");
    Value* rval = Rvalue->codegen();
    if (!rval)
        return nullptr;
    if (V->getType()->isFloatTy()) {
        if (rval->getType()->isInteger()) { // Cast to from Integer
            rval = Builder.CreateSIToFP(rval, Type::getFloatTy(TheContext),
                                        "to_floattmp");
        }
    } else {
        if (rval->getType()->isFloatTy()) {
            rval = Builder.CreateFPToSI(rval, Type::getInt32Ty(TheContext),
                                        "to_inttmp");
        }
    }
    return std::optional(
        Builder.CreateStore(rval, V)); // TODO Check if it works
}

static Value* FunctionCallASTnode::codegen() { // TODO check if works
    Function* CalleeF = TheModule->getFunction(Identifier);
    if (!CalleeF)
        return LogErrorV("No function found");

    // Check for argument mismatch
    if (CalleeF->arg_size() != Args.size())
        return LogErrorV("Incorrect number of arguments passed");
    std::vector<Value*> ArgsV;
    for (const auto& arg : Args) {
        ArgsV.push_back(arg->codegen());
        if (!ArgsV.back())
            return std::optional(nullptr);
    }
    return std::optional(Builder.CreateCall(CalleF, ArgsV, Identifier));
}

static std::optional<Value*> BodyASTnode::codegen() {
    Function* TheFunction = Builder.GetInsertBlock()->getParent();
    BasicBlock* BB = BasicBlock::Create(TheContext, "block", TheFunction);
    Builder.SetInsertPoint(BB);
    NamedValues.push_back(std::map<std::string, AllocaInst*>());
    for (const auto& Local : LocalD) {
        auto V = Local->codegen();
        if (V.has_value() && !V.value()) {
            return std::nullopt;
        }
    };
    for (const auto& Stmt : StmtL) { // TODO feels buggy
        auto V = Stmt->codegen();
        if (V.has_value() && !V.value()) {
            return std::nullopt;
        }
    }
    NamedValues.pop_back();
    return BB;
}

std::optional<Value*> IfStatementASTnode::codegen() {
    Function* TheFunction = Builder.GetInsertBlock()->GetParent();
    BasicBlock* true_     = BasicBlock::Create(TheContext, "then", TheFunction);
    BasicBlock* end_      = BasicBlock::Create(TheContext, "end");
    Value* cond           = Predicate->codegen();
    if (!cond)
        return misc::NULLOPTPTR;
    if (Else.has_value()) {
        BasicBlock* else_ = BasicBlock::Create(TheContext, "else");
        BasicBlock.CreateCondBr(comp, true_, else_);
        Builder.SetInsertPoint(else_);
        auto val = Else.value()->codegen();
        TheFunction->getBasicBlockList().push_back(else_);
        if (!val)
            return misc::NULLOPTPTR;
        Builder.CreateBr(end_);
    } else {
        BasicBlock.CreateCondBr(comp, true_, end_);
    }
    Builder.SetInsertPoint(true_);
    auto BodRes = Body->codegen();
    if (!BodRes)
        return misc::NULLOPTPTR;
    TheFunction->getBasicBlockList().push_back(end_);
    Builder.CreateBr(end_);
    Builder.SetInsertPoint(end_);
    return std::nullopt;
}

std::optional<Value*> FunctionASTnode::codegen() {
    std::variant<std::vector<Type*>, Void> ArgsT;
    if (std::holds_alternative<VectorAST>(Args)) {
        std::vector<Type*> tmp(Args.size());
        unsigned i = 0;
        for (const auto& Arg : std::get<VectorAST>(Args)) {
            ASTnode* f = Arg.get();
            auto f2    = static_cast<DeclarationASTnode*>(f);
            if (f2) {
                // >
            }
            // tmp[i++] = Arg -, TOKEN_TYPE::FLOAT_TOK
        }
    }
    return misc::NULLOPTPTR;
}
//===----------------------------------------------------------------------===//
// AST Printer
//===----------------------------------------------------------------------===//

inline llvm::raw_ostream& operator<<(llvm::raw_ostream& os,
                                     const ASTnode& ast) {
    os << ast.to_string();
    return os;
}

//===----------------------------------------------------------------------===//
// Main driver code.
//===----------------------------------------------------------------------===//

int main(int argc, char** argv) {
    if (argc == 2) {
        pFile = fopen(argv[1], "r");
        if (pFile == NULL)
            perror("Error opening file");
    } else {
        std::cout << "Usage: ./code InputFile\n";
        return 1;
    }

    // initialize line number and column numbers to zero
    lineNo   = 1;
    columnNo = 1;

    // get the first token
    getNextToken();
    // Tests LEXER
    // while (CurTok.type != EOF_TOK) {
    //   fprintf(stderr, "Token: %s with type %d\n",
    //   CurTok.lexeme.c_str(),
    //           CurTok.type);
    //   getNextToken();
    // }
    // fprintf(stderr, "Lexer Finished\n");

    // Report

    // Make the module, which holds all the code.
    TheModule = std::make_unique<Module>("mini-c", TheContext);

    // Run the parser now.
    parser();
    fprintf(stderr, "Parsing Finished\n");

    //********************* Start printing final IR
    //**************************
    // Print out all of the generated code into a file called output.ll
    auto Filename = "output.ll";
    std::error_code EC;
    raw_fd_ostream dest(Filename, EC, sys::fs::OF_None);

    if (EC) {
        errs() << "Could not open file: " << EC.message();
        return 1;
    }
    // TheModule->print(errs(), nullptr); // print IR to terminal
    TheModule->print(dest, nullptr);
    //********************* End printing final IR
    //****************************

    fclose(pFile); // close the file that contains the code that was parsed
    return 0;
}
