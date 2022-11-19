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

  public:
    virtual ~ASTnode();
    virtual std::optional<Value*> codegen() = 0;
    virtual std::string to_string(const size_t tab) const;
};

/// IntASTnode - Class for integer literals like 1, 2, 10,
class IntASTnode : public ASTnode {
    int Val;
    TOKEN Tok;

  public:
    IntASTnode(TOKEN tok, int val);
    virtual std::optional<Value*> codegen() override; // TODO Don't Rush
    // return a sting representation of this AST node
    virtual std::string to_string(const size_t tab) const override;
};

/// FloatASTnode - Class for floating literals
class FloatASTnode : public ASTnode {
    float Val;
    TOKEN Tok;

  public:
    FloatASTnode(TOKEN tok, float val);
    virtual std::optional<Value*> codegen() override;
    virtual std::string to_string(const size_t tab) const override;
};

using Void        = char;
using UPtrASTnode = std::unique_ptr<ASTnode>;
using OptionalPtr = std::optional<UPtrASTnode>;
using VectorAST   = std::vector<UPtrASTnode>;

/// BoolASTnode - Class for boolean
class BoolASTnode : public ASTnode {
    bool Val;
    TOKEN Tok;

  public:
    BoolASTnode(TOKEN tok, bool val);

    virtual std::optional<Value*> codegen() override;

    // return a sting representation of this AST node
    virtual std::string to_string(const size_t tab) const override;
};

/// IdentifierASTnode - Class for identifiers
class IdentifierASTnode : public ASTnode {
    std::string Val;
    TOKEN Tok;

  public:
    IdentifierASTnode(TOKEN tok, std::string val);
    virtual std::optional<Value*> codegen() override; // TODO Don't Rush
    // return a sting representation of this AST node
    virtual std::string to_string(const size_t tab) const override;
};

/// FunctionCallIdentifierASTnode - Class for function call
class FunctionCallASTnode : public ASTnode {
    std::string Identifier;
    VectorAST Args;
    TOKEN Tok;

  public:
    FunctionCallASTnode(TOKEN tok, std::string identifier, VectorAST args);
    virtual std::optional<Value*> codegen() override; // TODO Don't Rush
    // return a sting representation of this AST node
    virtual std::string to_string(const size_t tab) const override;
};

/// BinaryOperatorASTnode - Class for binary operator
class BinaryOperatorASTnode : public ASTnode {
    TOKEN Tok;
    TOKEN_TYPE Op;
    UPtrASTnode LHS, RHS;

  public:
    BinaryOperatorASTnode(TOKEN tok, TOKEN_TYPE op, UPtrASTnode LHS,
                          UPtrASTnode RHS);

    virtual std::optional<Value*> codegen() override;
    // return a sting representation of this AST node
    Value* lazy_or(Value* LHS_Value);
    Value* lazy_and(Value* LHS_Value);
    virtual std::string to_string(const size_t tab) const override;
};

/// UnaryOperatorASTnode - Class for unary operator
class UnaryOperatorASTnode : public ASTnode {
    TOKEN Tok;
    TOKEN_TYPE Op;
    UPtrASTnode Expr;

  public:
    UnaryOperatorASTnode(TOKEN tok, TOKEN_TYPE op, UPtrASTnode expr);

    virtual std::optional<Value*> codegen() override; // TODO Don't Rush
    virtual std::string to_string(const size_t tab) const override;
};

/// AssignmentASTnode - Class for assignment statements
class AssignmentASTnode : public ASTnode {
    std::string Name;
    UPtrASTnode Rvalue;
    TOKEN Tok;

  public:
    AssignmentASTnode(TOKEN tok, std::string Name, UPtrASTnode rvalue);

    virtual std::optional<Value*> codegen() override;
    // return a sting representation of this AST node
    virtual std::string to_string(const size_t tab) const override;
};

/// IfStatementASTnode - Class for If statements
class IfStatementASTnode : public ASTnode {
    UPtrASTnode Predicate;
    UPtrASTnode Body;
    OptionalPtr Else; /// Else Body
    TOKEN Tok;

  public:
    IfStatementASTnode(TOKEN tok, UPtrASTnode predicate, UPtrASTnode body,
                       OptionalPtr else_body);

    virtual std::optional<Value*> codegen() override; // TODO Don't Rush
    // return a sting representation of this AST node
    virtual std::string to_string(const size_t tab) const override;
};

/// WhileStatementASTnode - Class for If statements
class WhileStatementASTnode : public ASTnode {
    UPtrASTnode Predicate;
    UPtrASTnode Body;
    TOKEN Tok;

  public:
    WhileStatementASTnode(TOKEN tok, UPtrASTnode predicate, UPtrASTnode body);

    virtual std::optional<Value*> codegen() override; // TODO Don't Rush

    // return a sting representation of this AST node
    virtual std::string to_string(const size_t tab) const override;
};

/// DeclarationASTnode - Class for extern statements
class DeclarationASTnode : public ASTnode {
    std::string Ident;
    TOKEN_TYPE Type;
    bool IsGlobal;
    TOKEN Tok; // TODO

  public:
    DeclarationASTnode(TOKEN tok, std::string ident, TOKEN_TYPE type);

    virtual std::optional<Value*> codegen() override; // TODO Don't Rush

    bool getIsGlobal();
    void setIsGlobal(bool Gl);
    // return a sting representation of this AST node
    std::string getIdent() const;
    TOKEN_TYPE getType() const;
    virtual std::string to_string(const size_t tab) const override;
};

using VectorDeclAST = std::vector<std::unique_ptr<DeclarationASTnode>>;
using Args_t        = std::variant<VectorDeclAST, Void>;

/// @brief  BodyASTnode -  Class for body statements
class BodyASTnode : public ASTnode {
    VectorAST LocalD, StmtL;
    bool IsMain;

  public:
    BodyASTnode(VectorAST localD, VectorAST stmtL);

    virtual std::optional<Value*> codegen() override; // TODO Don't Rush

    void setIsMain(bool Val);

    bool getIsMain();

    // return a sting representation of this AST node
    virtual std::string to_string(const size_t tab) const override;
};

/// @brief ProgramASTnode - Class for the program statements
class ProgramASTnode : public ASTnode {
    VectorAST ExternL, DeclL;

  public:
    ProgramASTnode(VectorAST externL, VectorAST declL);

    virtual std::optional<Value*> codegen() override; // TODO Don't Rush

    // return a sting representation of this AST node
    virtual std::string to_string(const size_t tab) const override;
};

/// @brief FunctionDeclASTnode - Class for the extern decleration
class ExternFunctionDeclASTnode : public ASTnode {
    std::string Ident;
    TOKEN_TYPE RetType;
    Args_t Args;
    TOKEN Tok; // TODO

  public:
    ExternFunctionDeclASTnode(std::string Ident, TOKEN_TYPE type, Args_t args);
    // return a sting representation of this AST node
    virtual std::string to_string(const size_t tab) const override;
    virtual std::optional<Value*> codegen() override; // TODO Don't Rush
};

/// @brief FunctionASTnode - A class for function statements
class FunctionASTnode : public ASTnode {
    Args_t Args;
    UPtrASTnode Body;
    std::string Ident;
    TOKEN_TYPE Ret;
    TOKEN Tok;

  public:
    FunctionASTnode(Args_t args, UPtrASTnode body, std::string Ident,
                    TOKEN_TYPE ret);

    virtual std::string to_string(const size_t tab) const override;
    virtual std::optional<Value*> codegen() override; // TODO Don't Rush
};

/// @brief  ReturnStatementASTnode- A class for return statements
class ReturnStatementASTnode : public ASTnode {
    OptionalPtr RetExpr;

  public:
    TOKEN Tok;
    ReturnStatementASTnode(TOKEN tok, OptionalPtr ret_expr);

    virtual std::optional<Value*> codegen() override; // TODO Don't Rush
    virtual std::string to_string(const size_t tab) const override;
};
#endif