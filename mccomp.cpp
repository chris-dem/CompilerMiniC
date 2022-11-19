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
#include <set>
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

static void misc::restoreState(const TOKEN& prev) {
    putBackToken(CurTok);
    putBackToken(prev);
    getNextToken();
}

UPtrASTnode parse::LogError(const char* str) {
    std::stringstream ss;
    ss << str << " " << misc::TokToString(CurTok);
    fprintf(stderr, "Error: %s\n", ss.str().c_str());
    return nullptr;
}

template <typename T> std::unique_ptr<T> parse::LogErrorT(const char* str) {
    fprintf(stderr, "Error: %s\n", str);
    return nullptr;
}

template <typename T> std::optional<T> parse::LogErrorOpt(const char* str) {
    // fprintf(stderr, "Error: %s\n", str);
    LogError(str);
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
static std::optional<VectorAST> parse::ParseArgs() {
    // Assuming previously checked ident and ( and consumed both
    // FIRST SET OF PARSE ARGS
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
        return args;
    } else if (CurTok.type == TOKEN_TYPE::RPAR) { // epsilon
        return std::optional(std::move(args));
    }
    return parse::LogErrorOpt<VectorAST>("Invalid Token, expeceted expression");
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
    auto Ret = std::make_unique<FunctionCallASTnode>(cTok, std::move(Ident),
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
                PrevTok, std::move(PrevTok.lexeme), std::move(Expr));
            return std::move(Ret);
        } // if not it means not rval
        misc::restoreState(PrevTok);
    }
    // getNextToken();
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
        auto cTok = CurTok;
        getNextToken(); // Consume ||
        auto RHS = parse::ParseRAnd();
        if (!RHS)
            return nullptr;
        curr = std::make_unique<BinaryOperatorASTnode>(
            cTok, TOKEN_TYPE::OR, std::move(curr), std::move(RHS));
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
        auto cTok = CurTok;
        getNextToken(); // consume &&
        auto RHS = parse::ParseREq();
        if (!RHS)
            return nullptr;
        curr = std::make_unique<BinaryOperatorASTnode>(
            cTok, TOKEN_TYPE::AND, std::move(curr), std::move(RHS));
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
            PrevToken, static_cast<TOKEN_TYPE>(PrevToken.type), std::move(curr),
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
            PrevToken, static_cast<TOKEN_TYPE>(PrevToken.type), std::move(curr),
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
            PrevToken, static_cast<TOKEN_TYPE>(PrevToken.type), std::move(curr),
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
    while (CurTok.type == TOKEN_TYPE::DIV ||
           CurTok.type == TOKEN_TYPE::ASTERIX ||
           CurTok.type == TOKEN_TYPE::MOD) {
        auto PrevToken = CurTok;
        getNextToken(); // Absorb symbol
        auto RHS = parse::ParseRp();
        if (!RHS)
            return nullptr;
        curr = std::make_unique<BinaryOperatorASTnode>(
            PrevToken, static_cast<TOKEN_TYPE>(PrevToken.type), std::move(curr),
            std::move(RHS));
    }
    return std::move(curr);
}

/// rp ::= (expr)
///      | - rval
///      | ! rval
///      | IDENT
///      | IDENT (args)
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
        } // If LL2 failed, restore to previous state
        misc::restoreState(PrevTok);
        auto Ident = parse::ParseIdent();
        if (!Ident)
            return nullptr;
        return std::move(Ident);
    } else if (CurTok.type == TOKEN_TYPE::MINUS ||
               CurTok.type == TOKEN_TYPE::NOT) { // - expr | ! expr
        auto PrevTok = CurTok;
        getNextToken(); // Consume unary
        auto Expr = parse::ParseRp();
        if (!Expr)
            return nullptr;
        auto UnaryNode = std::make_unique<UnaryOperatorASTnode>(
            PrevTok, static_cast<TOKEN_TYPE>(PrevTok.type), std::move(Expr));
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
        getNextToken(); // Absorb SC
        return parse::StmtType('c');
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
        auto Res = parse::ParseIf();
        if (!Res)
            return false;
        return std::move(Res);
    } else if (CurTok.type == TOKEN_TYPE::RETURN) {
        auto Res = parse::ParseReturn();
        if (!Res)
            return false;
        return std::move(Res);
    } else if (CurTok.type == TOKEN_TYPE::WHILE) {
        auto Res = parse::ParseWhile();
        if (!Res)
            return false;
        return Res;
    } else if (CurTok.type == TOKEN_TYPE::LBRA) {
        auto Res = parse::ParseBlock();
        if (!Res)
            return false;
        return Res;
    }
    LogError("Invalid statement, expected if, while, return, block, "
             "expression statement or right bracket");
    return parse::StmtType(false);
}

static std::optional<VectorAST> parse::ParseStmtList() {
    auto StList = VectorAST();
    while (CurTok.type != TOKEN_TYPE::RBRA) {
        auto Stmt = parse::ParseStmt();
        if (std::holds_alternative<Semi>(Stmt)) // Indicates ;
            continue;
        if (std::holds_alternative<Err>(Stmt)) { // Error occured
            return std::nullopt;
        }
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
        // if (DeclarationASTnode* b =
        //         dynamic_cast<DeclarationASTnode*>(DeclNode.get()))
        //     b->setIsGlobal(true);
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
        auto Res = std::make_unique<DeclarationASTnode>(
            Ident, std::move(Ident.lexeme),
            static_cast<TOKEN_TYPE>(Var_Type.type));
        Res->setIsGlobal(true);
        return Res;
    } else if (CurTok.type == TOKEN_TYPE::LPAR) { // Fun decl
        auto Params = parse::ParseParams();       // () Consumed in parseblock
        if (!Params.has_value())
            return nullptr;
        auto block = parse::ParseBlock(); // {} Consumed in parseblock
        if (!block) {
            std::cout << "Here" << std::endl;
            return nullptr;
        }
        return std::make_unique<FunctionASTnode>(
            std::move(Params.value()), std::move(block),
            std::move(Ident.lexeme), static_cast<TOKEN_TYPE>(Var_Type.type));
    }
    return parse::LogError("Invalid declaration");
}

static std::unique_ptr<DeclarationASTnode> parse::ParseTypeIdent() {
    auto Var_Type = CurTok;
    if (!misc::checkTokenVarType(Var_Type.type))
        return parse::LogErrorT<DeclarationASTnode>("No type found");
    getNextToken(); // Consume Type
    auto Ident = CurTok;
    if (CurTok.type != TOKEN_TYPE::IDENT)
        return parse::LogErrorT<DeclarationASTnode>("No Identifier found");
    getNextToken(); // Consume name
    return std::make_unique<DeclarationASTnode>(
        Var_Type, std::move(Ident.lexeme),
        static_cast<TOKEN_TYPE>(Var_Type.type));
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
        auto Arg_list = VectorDeclAST(); // List of arguments
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
static std::unique_ptr<BodyASTnode> parse::ParseBlock() {
    if (CurTok.type != TOKEN_TYPE::LBRA) {
        parse::LogError("No left bracket found");
        return nullptr;
    }
    getNextToken(); // Consume LBRA
    auto LocalRes = parse::ParseLocalDelcs();
    if (!LocalRes.has_value()) // Error happened
        return nullptr;
    auto LocalD  = std::move(LocalRes.value());
    auto StmtRes = parse::ParseStmtList();
    if (!StmtRes.has_value())
        return nullptr;
    if (CurTok.type != TOKEN_TYPE::RBRA) {
        parse::LogError("No right bracket found");
        return nullptr;
    }
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
    block->setIsMain(true);
    return std::make_unique<FunctionASTnode>(
        std::move(Params.value()), std::move(block), std::move(Ident.lexeme),
        static_cast<TOKEN_TYPE>(RetType.type));
}

static UPtrASTnode parse::ParseIf() {
    if (CurTok.type != TOKEN_TYPE::IF)
        return parse::LogError("Expected If token");
    auto IfTok = CurTok;
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
        IfTok, std::move(Expr), std::move(Block), std::move(Else));
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
    auto WhileTok = CurTok;
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
    return std::make_unique<WhileStatementASTnode>(WhileTok, std::move(Expr),
                                                   std::move(Body));
}

/// return_stmt ::= "return" expr ";"
///              | "return" ;
static UPtrASTnode parse::ParseReturn() {
    if (CurTok.type != TOKEN_TYPE::RETURN) // Check for return
        return parse::LogError("Expected return token");
    auto RetToken = CurTok;
    getNextToken(); // Consume return
    std::optional<UPtrASTnode> Expr =
        std::nullopt; // Optional incase for no expression
    if (misc::checkTokenInGroup(CurTok.type)) // Check if exists expression
        Expr = std::optional<UPtrASTnode>(
            parse::ParseExpr()); // Parse expression and put in an optional
    if (CurTok.type != TOKEN_TYPE::SC) // Force to finish with SC
        return parse::LogError("Expected ; token");
    getNextToken(); // Parse ;
    return std::make_unique<ReturnStatementASTnode>(RetToken, std::move(Expr));
}

//===----------------------------------------------------------------------===//
static UPtrASTnode parser() {
    // add body
    auto Ret = parse::ParseProgram();
    if (!Ret) {
        llvm::errs() << "Error"
                     << "\n";
    } else {
        outs() << "Parsed Successfully"
               << "\n";
        outs() << Ret->to_string(0) << "\n";
    }
    return std::move(Ret);
}

//===----------------------------------------------------------------------===//
// Code Generation
//===----------------------------------------------------------------------===//

/// SINCE CODEGEN RETURNS std::optional<Value*>
/// ERROR IS DENOTED AS std::optional(nullptr)
/// Error could have also been nullopt but I preferred to use it as as a
/// function without the need to return

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

static std::map<std::string,
                GlobalVariable*>
    GlobalVariables; // Will keep of global variables

/// @brief Value to hold of the return value of a function. Usage of optional
/// for lazy instatiation, and for voids
static std::optional<AllocaInst*> RetValue = std::nullopt;
static BasicBlock* retBlock                = nullptr;

std::optional<Value*> LogErrorV(const char* str) {
    fprintf(stderr, "Error: %s\n", str);
    return misc::NULLOPTPTR;
}
std::optional<Value*> LogErrorV(std::string str) {
    fprintf(stderr, "Error: %s\n", str.c_str());
    return misc::NULLOPTPTR;
}
bool misc::checkValidExprType(llvm::Value* Val) {
    if (!Val)
        return false;
    if (Val->getType()->isVoidTy()) {
        return false;
    }
    return true;
}

llvm::Type* misc::convertToType(const TOKEN_TYPE& t) {
    switch (t) {
    case TOKEN_TYPE::BOOL_TOK:
    case TOKEN_TYPE::INT_TOK:
        return Type::getInt32Ty(TheContext);
    case TOKEN_TYPE::FLOAT_TOK:
        return Type::getFloatTy(TheContext);
    default:
        return nullptr;
    }
    return nullptr;
}

llvm::Value* misc::CastToi32(llvm::Value* val) {
    return Builder.CreateIntCast(val, Type::getInt32Ty(TheContext), false);
}

static AllocaInst* misc::findElemSC(const std::string& Name) {
    for (auto it = NamedValues.rbegin(); it != NamedValues.rend();
         it++) { // Start from the most local scope and go backwards
        if ((*it).find(Name) != (*it).end()) {
            return (*it)[Name];
        }
    }
    return nullptr;
}
static GlobalVariable* misc::findElemGl(const std::string& Name) {
    if (GlobalVariables.find(Name) != GlobalVariables.end())
        return GlobalVariables[Name];
    return nullptr;
}
static AllocaInst* misc::CreateEntryBlockAlloca(Function* TheFunction,
                                                const std::string& VarName,
                                                Type* typ) {
    IRBuilder<> TmpB(&TheFunction->getEntryBlock(),
                     TheFunction->getEntryBlock().begin());
    return TmpB.CreateAlloca(typ, 0, VarName.c_str());
}

/// @brief  Create Float Literal
/// Always return a value
/// @return Optional float literal
std::optional<Value*> FloatASTnode::codegen() {
    return std::optional(ConstantFP::get(TheContext, APFloat(Val)));
}

/// @brief  Create Integer Literal
/// Always return a value
/// Ints are strictly i32 signed
/// @return Optional int literal
std::optional<Value*> IntASTnode::codegen() {
    return std::optional(ConstantInt::get(TheContext, APInt(32, Val, true)));
}

/// @brief  Create Boolean Literal
/// Always return a value
/// Booleans in C are denoted as follows : 1 for true and 0 for false
/// Truthy values are all values != to 0
/// By C convention => convert true to 1 and 0 to false
/// @return Optional int literal
std::optional<Value*> BoolASTnode::codegen() {
    int val = Val ? 1 : 0;
    return std::optional(ConstantInt::get(TheContext, APInt(32, Val, true)));
}

/// @brief Return value of Identifier
/// Return value of Identifier
/// The lookup process goes as follows
/// Check the scope of the function, if not found, check global scope
/// If empty => return error
/// Else reutrn optional value loaded
/// @return optional loaded value
std::optional<Value*> IdentifierASTnode::codegen() {
    // Look up variable in function
    AllocaInst* V = misc::findElemSC(Val);
    if (V)
        return std::optional(Builder.CreateLoad(V->getAllocatedType(), V));

    auto Gl = misc::findElemGl(Val);
    if (Gl) {
        return std::optional(Builder.CreateLoad(Gl->getValueType(), Gl));
    }
    return LogErrorV(std::string("Unknown variable ") + misc::TokToString(Tok));
}
/// @brief Binary Operator node code generation
/// Two parts:
/// For strict evaluation operators (every operator other than && and ||),
/// evaluate both sides check is we had a case (void_function() + 3) => enforce
/// that void functions cannot return values. In addition upcast sides to the
/// type that will fit the most data, for example : int, int => int,int ||
/// float,float => float,float || (float , int | int,float) => float,float.
/// Reminder that boolean is treated as as i32 in both this language and C so no
/// need to account for additional case. For lazy operators, evaluate lhs and
/// depending on the type and result evaluate rhs,(more details on the
/// functions). Reminder, all comparison operators are casted to i32 since by
/// default they return i1 != i32. This causes issues depending on the context
/// and causes llvm to crash. To avoid this, enforce result to be i32. In i32 is
/// casted to unsigned because signed causes i1 true to be -1, which does not
/// make sense.
/// @return
std::optional<Value*> BinaryOperatorASTnode::codegen() {
    Value* L = LHS->codegen().value();
    if (misc::checkStrictEval(Op)) { // For strict operators
        Value* R = RHS->codegen().value();
        if (!L || !R) // Check if both sides are valid
            return misc::NULLOPTPTR;
        if (L->getType()->isVoidTy()) // Check if lhs is void
            return LogErrorV(
                std::string(
                    "Cannot assign return value from void function at line ") +
                std::to_string(Tok.lineNo) + " check LHS");
        if (R->getType()->isVoidTy()) // Check if rhs is void
            return LogErrorV(
                std::string(
                    "Cannot assign return value from void function at line ") +
                std::to_string(Tok.lineNo) + " check RHS");
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
        if (L->getType()->isFloatTy()) // Since both have the same
                                       // value, check
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
            case TOKEN_TYPE::MOD: // I denoted the modulo of floating numbers as
                                  // a semantic issue
                return LogErrorV(
                    std::string("Cannot use modulo on floating numbers at ") +
                    misc::TokToString(Tok));
                break;
            case TOKEN_TYPE::EQ:
                return std::optional(misc::CastToi32(Builder.CreateFCmpOEQ(
                    L, R, "f_eqtmp"))); // Enforce result to be i32
                break;
            case TOKEN_TYPE::NE:
                return std::optional(misc::CastToi32(Builder.CreateFCmpONE(
                    L, R, "f_netmp"))); // Enforce result to be i32
                break;
            case TOKEN_TYPE::LE:
                return std::optional(misc::CastToi32(Builder.CreateFCmpOLE(
                    L, R, "f_letmp"))); // Enforce result to be i32
                break;
            case TOKEN_TYPE::LT:
                return std::optional(misc::CastToi32(Builder.CreateFCmpOLT(
                    L, R, "f_lttmp"))); // Enforce result to be i32
                break;
            case TOKEN_TYPE::GE:
                return std::optional(misc::CastToi32(Builder.CreateFCmpOGE(
                    L, R, "f_getmp"))); // Enforce result to be i32
                break;
            case TOKEN_TYPE::GT:
                return std::optional(misc::CastToi32(Builder.CreateFCmpOGT(
                    L, R, "f_gttmp"))); // Enforce result to be i32
                break;
            default: // Impossible case
                return misc::NULLOPTPTR;
            }
        else // Else both int
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
            case TOKEN_TYPE::MOD:
                return std::optional(Builder.CreateSRem(L, R, "i_modtmp"));
                break;
            case TOKEN_TYPE::EQ:
                return std::optional(
                    misc::CastToi32(Builder.CreateICmpEQ(L, R, "i_eqtmp")));
                break;
            case TOKEN_TYPE::NE:
                return std::optional(
                    misc::CastToi32(Builder.CreateICmpNE(L, R, "i_netmp")));
                break;
            case TOKEN_TYPE::LE:
                return std::optional(
                    misc::CastToi32(Builder.CreateICmpSLE(L, R, "i_letmp")));
                break;
            case TOKEN_TYPE::LT:
                return std::optional(
                    misc::CastToi32(Builder.CreateICmpSLT(L, R, "i_lttmp")));
                break;
            case TOKEN_TYPE::GE:
                return std::optional(
                    misc::CastToi32(Builder.CreateICmpSGE(L, R, "i_getmp")));
                break;
            case TOKEN_TYPE::GT:
                return std::optional(
                    misc::CastToi32(Builder.CreateICmpSGT(L, R, "i_gttmp")));
                break;
            default:
                return misc::NULLOPTPTR;
            }
    }
    if (Op == TOKEN_TYPE::OR) { // Apply lazy or
        Value* or_val = lazy_or(L);
        if (!or_val)
            return misc::NULLOPTPTR;
        return std::optional(or_val);
    } // If operator is not in anything previous
    Value* and_val = lazy_and(L);
    if (!and_val)
        return misc::NULLOPTPTR;
    return std::optional(and_val);
}
/// @brief Lazy and evaluation
/// @param LHS value of LHS
/// The way lazy and works is as follows:
/// I create three blocks, true case, false case and merge case
/// Using the truthy value of LHS, If it is true => In the true block get the
/// value of the rhs, Else if false create a false value, and by using the
/// PHInode, get the phinode of the expression using the two blocks
/// PHInode made more sense to be used in this scenario since it is easier to
/// handle and there are a limited number of paths
/// @return
Value* BinaryOperatorASTnode::lazy_and(Value* LHS) {
    // Check if true
    Value* comp = misc::check_truthy(LHS);
    // Create the conditional blocks
    Function* TheFunction = Builder.GetInsertBlock()->getParent();
    BasicBlock* true_  = BasicBlock::Create(TheContext, "true_", TheFunction);
    BasicBlock* false_ = BasicBlock::Create(TheContext, "false_");
    BasicBlock* merge_ = BasicBlock::Create(TheContext, "merge_");
    // Depending of the value of the LHS, jump to true block, or to false
    Builder.CreateCondBr(comp, true_, false_);
    Builder.SetInsertPoint(true_);            // If true
    Value* true_val = RHS->codegen().value(); // Evaluate rhs
    if (!true_val)                            // Error
        return nullptr;
    true_val = misc::CastToi32(
        misc::check_truthy(true_val)); // Cast to i32 truthy value
    Builder.CreateBr(merge_);          // Create jump to merged out come
    true_ = Builder.GetInsertBlock(); // Set true_ as to the current insertblock
    TheFunction->getBasicBlockList().push_back(false_); // Create false_ block
    Builder.SetInsertPoint(false_); // Set insertion of data to false_
    Value* false_val = ConstantInt::get(
        TheContext, APInt(32, 0, true)); // Create 0 consant => false by c rules
    Builder.CreateBr(merge_);            // Jump to merged block
    false_ = Builder.GetInsertBlock();
    TheFunction->getBasicBlockList().push_back(merge_);
    Builder.SetInsertPoint(merge_);
    PHINode* PN = // Create a phinode with 2 possible values
        Builder.CreatePHI(Type::getInt32Ty(TheContext), 2, "lazy_and");
    PN->addIncoming(true_val, true_);   // true value from the true block
    PN->addIncoming(false_val, false_); // false value from the false block
    return PN;                          // return value
}

/// @brief Lazy and evaluation
/// @param LHS value of LHS
/// The way lazy and works is as follows:
/// I create three blocks, true case, false case and merge case
/// Using the truthy value of LHS, If it is true => In the true block I return
/// true value , Else if false return evaluated rhs, and by using the PHInode,
/// get the final result by choosing which phivalue to pick from which phi
/// block. Using the two blocks PHInode made more sense to be used in this
/// scenario since it is easier to handle and there are a limited number of
/// paths
/// @return
Value* BinaryOperatorASTnode::lazy_or(Value* LHS) {
    // Check if true
    Value* comp = misc::check_truthy(LHS);
    // Return optional value true
    Function* TheFunction = Builder.GetInsertBlock()->getParent();
    BasicBlock* true_  = BasicBlock::Create(TheContext, "true_", TheFunction);
    BasicBlock* false_ = BasicBlock::Create(TheContext, "false_");
    BasicBlock* merge_ = BasicBlock::Create(TheContext, "merge_");
    Builder.CreateCondBr(comp, true_, false_);
    Builder.SetInsertPoint(true_);
    Value* true_val =
        ConstantInt::get(TheContext, APInt(32, 1, true)); // Set to true
    Builder.CreateBr(merge_);
    true_ = Builder.GetInsertBlock();
    TheFunction->getBasicBlockList().push_back(false_);
    Builder.SetInsertPoint(false_);
    Value* false_val = RHS->codegen().value();
    if (!false_val) // Error
        return nullptr;
    false_val = misc::CastToi32(misc::check_truthy(false_val));
    Builder.CreateBr(merge_);
    false_ = Builder.GetInsertBlock();
    TheFunction->getBasicBlockList().push_back(merge_);
    Builder.SetInsertPoint(merge_);
    PHINode* PN = Builder.CreatePHI(Type::getInt32Ty(TheContext), 2, "lazy_or");
    PN->addIncoming(true_val, true_);
    PN->addIncoming(false_val, false_);
    return PN;
}

// Compare values if they are unequal to their corresponding type of 0
Value* misc::check_truthy(Value* LHS) {
    if (LHS->getType()->isFloatTy())
        return Builder.CreateFCmpONE(LHS,
                                     ConstantFP::get(TheContext, APFloat(0.0)));
    return Builder.CreateICmpNE(
        LHS, ConstantInt::get(TheContext, APInt(32, 0, true)));
}

/// @brief Unary node codegen
/// Same idea as above with the void values => force void function to not be
/// evaluated in expression
/// if floating and operator is neg => make neg
/// else convert to i1 and negate
/// @return
std::optional<Value*> UnaryOperatorASTnode::codegen() {
    Value* f = Expr->codegen().value(); // Expect codegen to always return value
    if (!f)
        return nullptr;
    if (f->getType()->isVoidTy())
        return LogErrorV(
            std::string("Cannot assign return value from void function at ") +
            misc::TokToString(Tok));
    if (f->getType()->isFloatTy()) {
        if (Op == TOKEN_TYPE::MINUS)
            return std::optional(Builder.CreateFNeg(f, "f_negtmp"));
        f = Builder.CreateFPToSI(f, Type::getInt32Ty(TheContext),
                                 "int_temp"); // Convert to int to apply not
    }
    switch (Op) {
    case TOKEN_TYPE::NOT:
        return std::optional(misc::CastToi32(Builder.CreateICmpEQ(
            f, ConstantInt::get(TheContext, APInt(32, 0, true)),
            "i_nottmp"))); // if 1 == 0 => 0 and if 0 == 0 =>  1
    case TOKEN_TYPE::MINUS:
        return std::optional(Builder.CreateNeg(f, "i_temp"));
    default:
        return misc::NULLOPTPTR;
    }
    return misc::NULLOPTPTR;
}

/// @brief Delcaration code generation
/// Made specifically for either local declarations or global declarations
/// @return
std::optional<Value*> DeclarationASTnode::codegen() {
    if (!IsGlobal) { // If value is not global
        if (NamedValues.back().find(Ident) !=
            NamedValues.back().find(
                Ident)) // Check if it was already declared in the local scope,
                        // and specifically in the most recent one
            return LogErrorV("Redeclaration of variable " + Ident + " " +
                             misc::TokToString(Tok));
        AllocaInst* var =
            misc::CreateEntryBlockAlloca(Builder.GetInsertBlock()->getParent(),
                                         Ident, misc::convertToType(Type));
        NamedValues.back()[Ident] = var; // Add value to the local scope
    } else {
        if (misc::findElemGl(Ident)) // Check if the variable was already
                                     // defined as a global variable
            return LogErrorV("Redeclaration of variable " + Ident + " " +
                             misc::TokToString(Tok));
        GlobalVariable* g;
        if (Type == TOKEN_TYPE::FLOAT_TOK) {
            g = new GlobalVariable(
                *(TheModule.get()), misc::convertToType(Type), false,
                GlobalValue::CommonLinkage,
                ConstantFP::get(Type::getFloatTy(TheContext), APFloat(0.0)));
        } else {
            g = new GlobalVariable(
                *(TheModule.get()), misc::convertToType(Type), false,
                GlobalValue::CommonLinkage,
                ConstantInt::get(Type::getInt32Ty(TheContext),
                                 APInt(32, 0, true)));
        }
        GlobalVariables[Ident] = g;
    }
    return std::nullopt; // No need to returna anything => nulloptional
}
/// @brief Code generation for the assignment
/// Get the value in the RHS of the equality
/// Implicitly convert the type of the result to the  type of the variable
/// @return
std::optional<Value*> AssignmentASTnode::codegen() {
    Value* rval = Rvalue->codegen().value();
    if (!rval) // Get the Rvalue
        return misc::NULLOPTPTR;
    if (rval->getType()->isVoidTy()) // Check if it was void
        return LogErrorV("Cannot assign return value from void function  " +
                         misc::TokToString(Tok));
    AllocaInst* V = misc::findElemSC(Name);
    if (!V) {
        GlobalVariable* Gl = misc::findElemGl(Name);
        if (!Gl)
            return LogErrorV("Variable not found " + misc::TokToString(Tok));

        if (Gl->getValueType()->isFloatTy()) {    // if global val stores floats
            if (rval->getType()->isIntegerTy()) { // Cast to from Integer
                rval = Builder.CreateSIToFP(rval, Type::getFloatTy(TheContext),
                                            "to_floattmp");
            }
        } else {
            if (rval->getType()
                    ->isFloatTy()) { // Vice verca for local declaration
                rval = Builder.CreateFPToSI(rval, Type::getInt32Ty(TheContext),
                                            "to_inttmp");
            }
        }
        Builder.CreateStore(rval, Gl); // Store the value in the global variable
        return std::optional(rval);    // return stored value
    }

    if (V->getAllocatedType()->isFloatTy()) {
        if (rval->getType()->isIntegerTy()) { // Cast to from Integer
            rval = Builder.CreateSIToFP(rval, Type::getFloatTy(TheContext),
                                        "to_floattmp");
        }
    } else {
        if (rval->getType()->isFloatTy()) {
            rval = Builder.CreateFPToSI(rval, Type::getInt32Ty(TheContext),
                                        "to_inttmp");
        }
    }
    Builder.CreateStore(rval, V); // Store the value in the local variable
    return std::optional(rval);   // return stored value
}
/// @brief Code generation for the functuon call
/// Check if the function exists, then check if the arguments match the function
/// type description. If they match check if the types are incompatible, throw a
/// warning and cast
/// Call function
/// @return
std::optional<Value*> FunctionCallASTnode::codegen() {

    Function* CalleeF = TheModule->getFunction(Identifier);
    if (!CalleeF)
        return LogErrorV("No function found");

    // Check for argument mismatch
    if (CalleeF->arg_size() != Args.size())
        return LogErrorV("Incorrect number of arguments passed " +
                         misc::TokToString(Tok));
    std::vector<Value*> ArgsV;
    auto FArgs = CalleeF->getFunctionType()->params(); // get types of functions
    size_t idx = 0;
    for (const auto& arg : Args) {
        auto Val = arg->codegen().value();
        if (Val->getType() != FArgs[idx]) { // Check if incompatible types
            std::cout
                << "WARNING: Implicit conversion at function call at function "
                << Identifier << " line " << std::to_string(Tok.lineNo)
                << " at " << idx << " parameter" << std::endl;
            if (FArgs[idx]
                    ->isFloatTy()) { // If arg is float and val is int => upcast
                Val = Builder.CreateSIToFP(Val, Type::getFloatTy(TheContext),
                                           "f_iarg");
            } else { // => Else downcast
                Val = Builder.CreateFPToSI(Val, Type::getInt32Ty(TheContext),
                                           "i_farg");
            }
        }
        idx++;
        ArgsV.push_back(Val);
        if (!ArgsV.back())
            return misc::NULLOPTPTR;
    }
    return std::optional(Builder.CreateCall(CalleeF, ArgsV));
}

/// @brief Code generation for the ASTnode
/// Push a new map at the end of the list to indicate deepest scope
/// Code gen for local delc and check for errors
/// For statements :  Generate the code for every statement. Even if the
/// statement is a return, I still allow for the rest of the code to be
/// generated, to check for any semnatic errors
/// @return
std::optional<Value*> BodyASTnode::codegen() {
    // if (!IsMain) { // TODO If time do

    NamedValues.push_back(std::map<std::string, AllocaInst*>());
    // }
    for (const auto& Local : LocalD) {
        auto V = Local->codegen();
        if (V.has_value() && !V.value()) {
            return misc::NULLOPTPTR;
        }
    };
    for (size_t i = 0; i < StmtL.size(); i++) {
        auto V = StmtL[i]->codegen();
        if (V.has_value() && !V.value()) {
            return misc::NULLOPTPTR;
        }
    }
    // if(!IsMain) {
    NamedValues.pop_back();
    // }
    return std::nullopt; // If all good return nulloptional to indicate
                         // everything is fine
}

/// @brief If statement code generation
/// If->then works the same way as in the lecture slides
/// Extra precaussion in the types to ensure comparison is always between i32
/// and i32. Usage of (value in cond) != false to convert i32 to i1. Else blocks
/// works by create the block for the else, make the cond jump between true and
/// else body, generate code for true and jump to after. Since then block was
/// fully genertaed push back the else code and un ump to after and continue
/// @return
std::optional<Value*> IfStatementASTnode::codegen() {
    Function* TheFunction = Builder.GetInsertBlock()->getParent();
    BasicBlock* true_     = BasicBlock::Create(TheContext, "then", TheFunction);
    BasicBlock* end_      = BasicBlock::Create(TheContext, "end");
    Value* cond = Predicate->codegen().value(); // should be non optional
    if (cond->getType()->isFloatTy()) { // If the predicate is floating convert
                                        // to int for boolean comparison
        cond = Builder.CreateFPToSI(cond, Type::getInt32Ty(TheContext));
    }
    Value* comp = Builder.CreateICmpNE(
        cond, ConstantInt::get(TheContext, APInt(32, 0, true))); // APInt 0
                                                                 // => false
    if (!comp) {
        return misc::NULLOPTPTR;
    }
    if (Else.has_value()) {
        BasicBlock* else_ = BasicBlock::Create(TheContext, "else");
        Builder.CreateCondBr(comp, true_, else_);
        Builder.SetInsertPoint(true_);
        auto BodRes = Body->codegen();
        if (BodRes.has_value() && !BodRes.value()) {
            return misc::NULLOPTPTR;
        }
        TheFunction->getBasicBlockList().push_back(else_);
        Builder.CreateBr(end_);
        Builder.SetInsertPoint(else_);
        auto val = Else.value()->codegen();
        if (val.has_value() && !val.value())
            return misc::NULLOPTPTR;
    } else {
        Builder.CreateCondBr(comp, true_, end_);
        Builder.SetInsertPoint(true_);
        auto BodRes = Body->codegen();
        if (BodRes.has_value() && !BodRes.value()) {
            return misc::NULLOPTPTR;
        }
    }
    TheFunction->getBasicBlockList().push_back(end_);
    Builder.CreateBr(end_);
    Builder.SetInsertPoint(end_);
    return std::nullopt;
}

std::optional<Value*> WhileStatementASTnode::codegen() {
    Function* TheFunction = Builder.GetInsertBlock()->getParent();
    BasicBlock* header_ = BasicBlock::Create(TheContext, "header", TheFunction);
    BasicBlock* while_  = BasicBlock::Create(TheContext, "body");
    BasicBlock* after_  = BasicBlock::Create(TheContext, "after");
    Builder.CreateBr(header_);
    Builder.SetInsertPoint(header_);
    auto PredVal = Predicate->codegen().value();
    if (!PredVal)
        return misc::NULLOPTPTR;
    if (PredVal->getType()->isFloatTy()) {
        PredVal = Builder.CreateFPToSI(PredVal, Type::getInt32Ty(TheContext));
    }
    Value* comp = Builder.CreateICmpNE(
        PredVal, ConstantInt::get(TheContext, APInt(32, 0, true)),
        "while_comp");
    Builder.CreateCondBr(comp, while_, after_);
    TheFunction->getBasicBlockList().push_back(while_);
    Builder.SetInsertPoint(while_);

    std::optional<Value*> ResBody = Body->codegen();
    if (ResBody.has_value() && !ResBody.value())
        return misc::NULLOPTPTR;
    Builder.CreateBr(header_);
    TheFunction->getBasicBlockList().push_back(after_);
    Builder.SetInsertPoint(after_);
    return std::nullopt;
}

std::optional<Value*> ExternFunctionDeclASTnode::codegen() {
    //------------GENERATE PROTOTOTYPE------------
    std::vector<Type*> ArgsT;
    if (std::holds_alternative<VectorDeclAST>(Args)) {
        const auto& vArg = std::get<VectorDeclAST>(Args);
        std::vector<Type*> tmp;
        for (const auto& Arg : vArg) {
            if (Arg->getType() == TOKEN_TYPE::FLOAT_TOK) {
                tmp.push_back(Type::getFloatTy(TheContext));
            } else {
                tmp.push_back(Type::getInt32Ty(TheContext));
            }
        }
        ArgsT = tmp;
    }
    Type* VRetType;
    switch (RetType) {
    case TOKEN_TYPE::INT_TOK:
    case TOKEN_TYPE::BOOL_TOK:
        VRetType = Type::getInt32Ty(TheContext);
        break;
    case TOKEN_TYPE::FLOAT_TOK:
        VRetType = Type::getFloatTy(TheContext);
        break;
    default:
        VRetType = Type::getVoidTy(TheContext);
    }
    FunctionType* MyFT = FunctionType::get(VRetType, ArgsT, false);
    if (!MyFT)
        return LogErrorV("Error creating function type");
    Function* F = Function::Create(MyFT, Function::ExternalLinkage, Ident,
                                   TheModule.get());
    if (!F)
        return LogErrorV("Error creating function");
    return std::nullopt;
}

std::optional<Value*> FunctionASTnode::codegen() {
    //------------GENERATE PROTOTOTYPE------------
    std::vector<Type*> ArgsT;
    if (std::holds_alternative<VectorDeclAST>(Args)) {
        const auto& vArg = std::get<VectorDeclAST>(Args);
        std::vector<Type*> tmp;
        for (const auto& Arg : vArg) {
            if (Arg->getType() == TOKEN_TYPE::FLOAT_TOK) {
                tmp.push_back(Type::getFloatTy(TheContext));
            } else {
                tmp.push_back(Type::getInt32Ty(TheContext));
            }
        }
        ArgsT = tmp;
    }
    Type* VRetType;
    switch (Ret) {
    case TOKEN_TYPE::INT_TOK:
    case TOKEN_TYPE::BOOL_TOK:
        VRetType = Type::getInt32Ty(TheContext);
        break;
    case TOKEN_TYPE::FLOAT_TOK:
        VRetType = Type::getFloatTy(TheContext);
        break;
    default:
        VRetType = Type::getVoidTy(TheContext);
    }
    FunctionType* MyFT = FunctionType::get(VRetType, ArgsT, false);
    // DefinedFunctions.insert(Ident); // Add function add defined function
    Function* F = Function::Create(MyFT, Function::ExternalLinkage, Ident,
                                   TheModule.get());

    BasicBlock* BB = BasicBlock::Create(TheContext, "entry", F);
    if (Ret != TOKEN_TYPE::VOID_TOK)
        RetValue = std::optional(misc::CreateEntryBlockAlloca(
            F, "ret_val", misc::convertToType(Ret)));
    Builder.SetInsertPoint(BB);

    //--------------END OF PROTOTYPE--------------
    std::map<std::string, AllocaInst*> my_map; // Add values
    size_t idx = 0;
    for (auto& Arg : F->args()) {
        const VectorDeclAST& f = std::get<VectorDeclAST>(Args);
        Type* t                = misc::convertToType(f[idx]->getType());
        if (!t)
            return misc::NULLOPTPTR;

        AllocaInst* Alloca =
            misc::CreateEntryBlockAlloca(F, f[idx]->getIdent(), t);
        Builder.CreateStore(&Arg, Alloca);
        my_map[f[idx]->getIdent()] = Alloca;
        Arg.setName(f[idx++]->getIdent());
    }
    NamedValues.push_back(std::move(my_map));
    retBlock = BasicBlock::Create(TheContext, "ret");
    // Basic block code-gen
    // Set BasicBlock to true
    auto ret = Body->codegen();
    if (!ret.has_value() || ret.value()) {
        NamedValues.pop_back();
        // if (RetValue.has_value()) {
        //     Value* ValToRet =
        //         Builder.CreateLoad(misc::convertToType(Ret),
        //         RetValue.value());
        //     Builder.CreateRet(ValToRet);
        // } else {                     // if nulloptional
        //     Builder.CreateRetVoid(); // CREATE RIP
        // }
        Builder.CreateBr(retBlock);
        F->getBasicBlockList().push_back(retBlock);
        Builder.SetInsertPoint(retBlock);
        if (RetValue.has_value()) {
            Value* ValToRet =
                Builder.CreateLoad(misc::convertToType(Ret), RetValue.value());
            Builder.CreateRet(ValToRet);
        } else {                     // if nulloptional
            Builder.CreateRetVoid(); // CREATE RIP
        }
        if (verifyFunction(*F, &outs())) {
            std::cout << "Error validation" << std::endl;
            return misc::NULLOPTPTR;
        }
        // Nullify output
        RetValue.reset();
        return std::nullopt;
    }
    F->eraseFromParent();
    return misc::NULLOPTPTR;
}

std::optional<Value*> ProgramASTnode::codegen() {
    for (const auto& Ex : ExternL) {
        auto Res = Ex->codegen();
        if (Res.has_value() && !Res.value()) {
            return misc::NULLOPTPTR;
        }
    }
    for (const auto& Decl : DeclL) {
        auto Res = Decl->codegen();
        if (Res.has_value() && !Res.value()) {
            return misc::NULLOPTPTR;
        }
    }
    return std::nullopt;
}

std::optional<Value*> ReturnStatementASTnode::codegen() {
    if (RetValue.has_value()) {   // Return with expression
        if (!RetExpr.has_value()) // Return type has void
            return LogErrorV(
                "Function of type non-void does not return anything");
        auto retval = RetExpr.value()
                          ->codegen()
                          .value(); // Will always return with expression
        if (!retval)
            return misc::NULLOPTPTR;
        if (RetValue.value()->getAllocatedType()->getTypeID() !=
            retval->getType()->getTypeID())
            return LogErrorV("Incompatible return types");
        Builder.CreateStore(retval, RetValue.value());
    } else {                     // If ret type is void
        if (RetExpr.has_value()) // and the returns tatement returns
                                 // with expression
            return LogErrorV("Functon of type void returns expression");
    }
    auto AlwaysT     = Builder.getInt1(true);
    BasicBlock* Dead = BasicBlock::Create(
        TheContext, "deadcode", Builder.GetInsertBlock()->getParent());
    Builder.CreateCondBr(AlwaysT, retBlock, Dead);
    Builder.SetInsertPoint(Dead);
    return std::nullopt;
}

//===----------------------------------------------------------------------===//
// AST Printer
//===----------------------------------------------------------------------===//

inline llvm::raw_ostream& operator<<(llvm::raw_ostream& os,
                                     const ASTnode& ast) {
    os << ast.to_string(0);
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
    auto Tree = parser();
    if (Tree) {
        fprintf(stderr, "Parsing Finished\n");
    } else {
        return -1; // Error
    }

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
    auto Res = Tree->codegen();
    if (Res.has_value() && !Res.value()) {
        errs() << "Error";
    } else {
        std::cout << "Made it out" << std::endl;
    } //
    TheModule->print(dest, nullptr);
    //********************* End printing final IR
    //****************************

    fclose(pFile); // close the file that contains the code that was parsed
    return 0;
}