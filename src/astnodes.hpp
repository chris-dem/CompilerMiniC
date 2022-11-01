#ifndef ASTNODES_H
#define ASTNODES_H

// #include "helpers.hpp"
#include "llvm/IR/DerivedTypes.h"
#include <memory>
#include <optional>
#include <string>
#include <variant>
#include <vector>

using namespace llvm;
using namespace llvm::sys;

/* add other AST nodes as nessasary */
// The lexer returns one of these for known things.
enum TOKEN_TYPE {

    IDENT  = -1,       // [a-zA-Z_][a-zA-Z_0-9]*
    ASSIGN = int('='), // '='

    // delimiters
    LBRA  = int('{'), // left brace
    RBRA  = int('}'), // right brace
    LPAR  = int('('), // left parenthesis
    RPAR  = int(')'), // right parenthesis
    SC    = int(';'), // semicolon
    COMMA = int(','), // comma

    // types
    INT_TOK   = -2, // "int"
    VOID_TOK  = -3, // "void"
    FLOAT_TOK = -4, // "float"
    BOOL_TOK  = -5, // "bool"

    // keywords
    EXTERN = -6,  // "extern"
    IF     = -7,  // "if"
    ELSE   = -8,  // "else"
    WHILE  = -9,  // "while"
    RETURN = -10, // "return"
    // TRUE   = -12,     // "true"
    // FALSE   = -13,     // "false"

    // literals
    INT_LIT   = -14, // [0-9]+
    FLOAT_LIT = -15, // [0-9]+.[0-9]+
    BOOL_LIT  = -16, // "true" or "false" key words

    // logical operators
    AND = -17, // "&&"
    OR  = -18, // "||"

    // operators
    PLUS    = int('+'), // addition or unary plus
    MINUS   = int('-'), // substraction or unary negative
    ASTERIX = int('*'), // multiplication
    DIV     = int('/'), // division
    MOD     = int('%'), // modular
    NOT     = int('!'), // unary negation

    // comparison operators
    EQ = -19,      // equal
    NE = -20,      // not equal
    LE = -21,      // less than or equal to
    LT = int('<'), // less than
    GE = -23,      // greater than or equal to
    GT = int('>'), // greater than

    // special tokens
    EOF_TOK = 0, // signal end of file

    // invalid
    INVALID = -100 // signal invalid token
};

// TOKEN struct is used to keep track of information about a token
struct TOKEN {
    int type = -100; // By default error
    std::string lexeme;
    int lineNo;
    int columnNo;
};

/// ASTnode - Base class for all AST nodes.
class ASTnode {
    std::string Name;

  public:
    virtual ~ASTnode();
    virtual Value* codegen();
    virtual std::string to_string() const;
};

/// IntASTnode - Class for integer literals like 1, 2, 10,
class IntASTnode : public ASTnode {
    int Val;
    TOKEN Tok;

  public:
    IntASTnode(TOKEN tok, int val);
    virtual Value* codegen() override; // TODO Don't Rush
    // return a sting representation of this AST node
    virtual std::string to_string() const override;
};

/// FloatASTnode - Class for floating literals
class FloatASTnode : public ASTnode {
    float Val;
    TOKEN Tok;

  public:
    FloatASTnode(TOKEN tok, float val);
    virtual Value* codegen() override;
    virtual std::string to_string() const override;
};

using Void        = char;
using UPtrASTnode = std::unique_ptr<ASTnode>;
using OptionalPtr = std::optional<UPtrASTnode>;
using VectorAST   = std::vector<UPtrASTnode>;
using Args_t      = std::variant<VectorAST, Void>;

/// BoolASTnode - Class for boolean
class BoolASTnode : public ASTnode {
    bool Val;
    TOKEN Tok;

  public:
    BoolASTnode(TOKEN tok, bool val);

    virtual Value* codegen() override;

    // return a sting representation of this AST node
    virtual std::string to_string() const override;
};

/// IdentifierASTnode - Class for identifiers
class IdentifierASTnode : public ASTnode {
    std::string Val;
    TOKEN Tok;

  public:
    IdentifierASTnode(TOKEN tok, std::string val);
    virtual Value* codegen() override; // TODO Don't Rush
    // return a sting representation of this AST node
    virtual std::string to_string() const override;
};

/// FunctionCallIdentifierASTnode - Class for function call
class FunctionCallASTnode : public ASTnode {
    std::string Identifier;
    VectorAST Args;

  public:
    FunctionCallASTnode(std::string identifier, VectorAST args);
    // virtual Value* codegen() override; // TODO Don't Rush
    // return a sting representation of this AST node
    virtual std::string to_string() const override;
};

/// BinaryOperatorASTnode - Class for binary operator
class BinaryOperatorASTnode : public ASTnode {
    TOKEN_TYPE Op;
    UPtrASTnode LHS, RHS;

  public:
    BinaryOperatorASTnode(TOKEN_TYPE op, UPtrASTnode LHS, UPtrASTnode RHS);

    virtual Value* codegen() override;
    // return a sting representation of this AST node
    virtual std::string to_string() const override;
};

/// UnaryOperatorASTnode - Class for unary operator
class UnaryOperatorASTnode : public ASTnode {
    TOKEN_TYPE Op;
    UPtrASTnode Expr;

  public:
    UnaryOperatorASTnode(TOKEN_TYPE op, UPtrASTnode expr);

    virtual Value* codegen() override; // TODO Don't Rush
    virtual std::string to_string() const override;
};

/// AssignmentASTnode - Class for assignment statements
class AssignmentASTnode : public ASTnode {
    std::string Name;
    UPtrASTnode Rvalue;

  public:
    AssignmentASTnode(std::string Name, UPtrASTnode rvalue);

    // virtual Value* codegen() override; // TODO Don't Rush
    // return a sting representation of this AST node
    virtual std::string to_string() const override;
};

/// IfStatementASTnode - Class for If statements
class IfStatementASTnode : public ASTnode {
    UPtrASTnode Predicate;
    UPtrASTnode Body;
    OptionalPtr Else; /// Else Body

  public:
    IfStatementASTnode(UPtrASTnode predicate, UPtrASTnode body,
                       OptionalPtr else_body);

    // virtual Value* codegen() override; // TODO Don't Rush
    // return a sting representation of this AST node
    virtual std::string to_string() const override;
};

/// WhileStatementASTnode - Class for If statements
class WhileStatementASTnode : public ASTnode {
    UPtrASTnode Predicate;
    UPtrASTnode Body;

  public:
    WhileStatementASTnode(UPtrASTnode predicate, UPtrASTnode body);

    // virtual Value* codegen() override; // TODO Don't Rush

    // return a sting representation of this AST node
    virtual std::string to_string() const override;
};

/// DeclarationASTnode - Class for extern statements
class DeclarationASTnode : public ASTnode {
    std::string Ident;
    TOKEN_TYPE Type;

  public:
    DeclarationASTnode(std::string ident, TOKEN_TYPE type);

    // virtual Value* codegen() override; // TODO Don't Rush

    // return a sting representation of this AST node
    virtual std::string to_string() const override;
};

/// @brief  BodyASTnode -  Class for body statements
class BodyASTnode : public ASTnode {
    VectorAST LocalD, StmtL;

  public:
    BodyASTnode(VectorAST localD, VectorAST stmtL);

    // virtual Value* codegen() override; // TODO Don't Rush

    // return a sting representation of this AST node
    virtual std::string to_string() const override;
};

/// @brief ProgramASTnode - Class for the program statements
class ProgramASTnode : public ASTnode {
    VectorAST ExternL, DeclL;

  public:
    ProgramASTnode(VectorAST externL, VectorAST declL);

    // virtual Value* codegen() override; // TODO Don't Rush

    // return a sting representation of this AST node
    virtual std::string to_string() const override;
};

/// @brief FunctionDeclASTnode - Class for the extern decleration
class ExternFunctionDeclASTnode : public ASTnode {
    std::string Ident;
    TOKEN_TYPE RetType;
    Args_t Args;

  public:
    ExternFunctionDeclASTnode(std::string Ident, TOKEN_TYPE type, Args_t args);
    // return a sting representation of this AST node
    virtual std::string to_string() const override;
};

/// @brief FunctionASTnode - A class for function statements
class FunctionASTnode : public ASTnode {
    Args_t Args;
    UPtrASTnode Body;
    std::string Ident;
    TOKEN_TYPE Ret;

  public:
    FunctionASTnode(Args_t args, UPtrASTnode body, std::string Ident,
                    TOKEN_TYPE ret);

    virtual std::string to_string() const override;
};

/// @brief  ReturnStatementASTnode- A class for return statements
class ReturnStatementASTnode : public ASTnode {
    OptionalPtr RetExpr;

  public:
    ReturnStatementASTnode(OptionalPtr ret_expr);

    // virtual std::string to_string() const override;
};
#endif