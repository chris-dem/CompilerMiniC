#include "astnodes.hpp"
#include "helpers.hpp"
#include <iostream>
#include <sstream>
#include <string>

//===----------------------------------------------------------------------===//
// ASTnode

ASTnode::~ASTnode() {}

std::string ASTnode::to_string(const size_t tab) const {
    return std::string("");
}

//===----------------------------------------------------------------------===//
//===----------------------------------------------------------------------===//
// IntASTnode
IntASTnode::IntASTnode(TOKEN tok, int val) : Tok(tok), Val(val) {}

std::string IntASTnode::to_string(const size_t tab) const {
    std::stringstream ss;
    ss << std::string(tab, '\t') << "[Integer literal node, Value : ";
    ss << Val;
    ss << " " << misc::TokToString(Tok) << " ]";
    return ss.str();
}
//===----------------------------------------------------------------------===//

//===----------------------------------------------------------------------===//
// FloatASTnode
FloatASTnode::FloatASTnode(TOKEN tok, float val) : Tok(tok), Val(val) {}

std::string FloatASTnode::to_string(const size_t tab) const {
    std::stringstream ss;
    ss << std::string(tab, '\t') << "[Floating literal node, Value : ";
    ss << Val;
    ss << " " << misc::TokToString(Tok) << " ]";
    return ss.str();
}
//===----------------------------------------------------------------------===//

//===----------------------------------------------------------------------===//
// BoolASTnode
BoolASTnode::BoolASTnode(TOKEN tok, bool val) : Tok(tok), Val(val) {}

std::string BoolASTnode::to_string(const size_t tab) const {
    std::stringstream ss;
    ss << std::string(tab, '\t') << "[Boolean literal node : Value : ";
    ss << Val;
    ss << " " << misc::TokToString(Tok) << " ]";
    return ss.str();
}
//===----------------------------------------------------------------------===//

//===----------------------------------------------------------------------===//
// IdentifierASTnode
IdentifierASTnode::IdentifierASTnode(TOKEN tok, std::string val)
    : Tok(tok), Val(std::move(val)) {}

std::string IdentifierASTnode::to_string(const size_t tab) const {
    std::stringstream ss;
    ss << std::string(tab, '\t') << "[Identifier literal node : Value : ";
    ss << Val;
    ss << " " << misc::TokToString(Tok) << "]";
    return ss.str();
}
//===----------------------------------------------------------------------===//

//===----------------------------------------------------------------------===//
// FunctionCallASTnode
FunctionCallASTnode::FunctionCallASTnode(TOKEN tok, std::string identifier,
                                         VectorAST args)
    : Identifier(identifier), Args(std::move(args)), Tok(tok) {}

std::string FunctionCallASTnode::to_string(const size_t tab) const {
    std::stringstream ss;
    ss << std::string(tab, '\t')
       << "[FunctionCall literal node : Identifier : " << Identifier << " "
       << misc::TokToString(Tok) << " Args: [" << std::endl;
    for (const auto& t : Args) {
        ss << t->to_string(tab + 1) << "\n";
    }
    ss << std::string(tab, '\t') << "] ]";
    return ss.str();
}
//===----------------------------------------------------------------------===////===----------------------------------------------------------------------===//

//===----------------------------------------------------------------------===//
// BinaryOperatorASTnode
BinaryOperatorASTnode::BinaryOperatorASTnode(TOKEN tok, TOKEN_TYPE op,
                                             UPtrASTnode LHS, UPtrASTnode RHS)
    : Tok(tok), Op(op), LHS(std::move(LHS)), RHS(std::move(RHS)) {}

std::string BinaryOperatorASTnode::to_string(const size_t tab) const {
    std::stringstream ss;
    ss << std::string(tab, '\t')
       << "[BinaryOperator literal node : " << misc::TokToString(Tok)
       << " Operator : " << misc::convertTokenType(Op) << " LHS : ["
       << std::endl
       << LHS->to_string(tab + 1) << std::endl
       << std::string(tab, '\t') << "] RHS : [" << std::endl
       << RHS->to_string(tab + 1) << std::endl
       << std::string(tab, '\t') << "] ]";
    return ss.str();
}
//===----------------------------------------------------------------------===//

//===----------------------------------------------------------------------===//
// UnaryOperatorASTnode
UnaryOperatorASTnode::UnaryOperatorASTnode(TOKEN tok, TOKEN_TYPE op,
                                           UPtrASTnode expr)
    : Tok(tok), Op(op), Expr(std::move(expr)) {}

std::string UnaryOperatorASTnode::to_string(const size_t tab) const {
    std::stringstream ss;
    ss << std::string(tab, '\t')
       << "[Unary literal node : " << misc::TokToString(Tok)
       << " Operator : " << misc::convertTokenType(Op) << " Expr : ["
       << std::endl
       << Expr->to_string(tab + 1) << std::endl
       << std::string(tab, '\t') << "] ]";
    return ss.str();
}
//===----------------------------------------------------------------------===//

//===----------------------------------------------------------------------===//
// AssignmentASTnode
AssignmentASTnode::AssignmentASTnode(TOKEN tok, std::string Name,
                                     UPtrASTnode rvalue)
    : Tok(tok), Name(std::move(Name)), Rvalue(std::move(rvalue)) {}

std::string AssignmentASTnode::to_string(const size_t tab) const {
    std::stringstream ss;
    ss << std::string(tab, '\t')
       << "[Assignement node : " << misc::TokToString(Tok)
       << " Ident : " << Name << " Expr : [ " << std::endl
       << Rvalue->to_string(tab + 1) << std::endl
       << std::string(tab, '\t') << "] ]";
    return ss.str();
}
//===----------------------------------------------------------------------===//

//===----------------------------------------------------------------------===//
// IfStatementASTnode
IfStatementASTnode::IfStatementASTnode(TOKEN tok, UPtrASTnode predicate,
                                       UPtrASTnode body, OptionalPtr else_body)
    : Tok(tok), Predicate(std::move(predicate)), Body(std::move(body)),
      Else(std::move(else_body)) {}

std::string IfStatementASTnode::to_string(const size_t tab) const {
    std::stringstream ss;
    ss << std::string(tab, '\t')
       << "[IfStatement node: " << misc::TokToString(Tok) << " Predicate : ["
       << std::endl
       << Predicate->to_string(tab + 1) << std::endl
       << std::string(tab, '\t') << "] Body : " << std::endl
       << Body->to_string(tab + 1) << std::endl
       << std::string(tab, '\t') << "ElseBody : [" << std::endl;
    if (Else.has_value()) {
        ss << Else.value()->to_string(tab + 1);
    } else {
        ss << std::string(tab + 1, '\t') << "No Else Body";
    }
    ss << std::string(tab, '\t') << "] ]";
    return ss.str();
}
//===----------------------------------------------------------------------===//

//===----------------------------------------------------------------------===//
// WhileStatementASTnode

WhileStatementASTnode::WhileStatementASTnode(TOKEN tok, UPtrASTnode predicate,
                                             UPtrASTnode body)
    : Tok(tok), Predicate(std::move(predicate)), Body(std::move(body)) {}

std::string WhileStatementASTnode::to_string(const size_t tab) const {
    std::stringstream ss;
    ss << std::string(tab, '\t') << "WhileStatement node"
       << misc::TokToString(this->Tok) << " Predicate : " << std::endl
       << Predicate->to_string(tab + 1) << std::endl
       << std::string(tab, '\t') << "Body : [" << std::endl;
    ss << Body->to_string(tab + 1);
    ss << std::endl << std::string(tab, '\t') << "] ]";
    return ss.str();
}
//===----------------------------------------------------------------------===//

//===----------------------------------------------------------------------===//
// DeclarationASTnode
DeclarationASTnode::DeclarationASTnode(TOKEN tok, std::string ident,
                                       TOKEN_TYPE type)
    : Tok(tok), Ident(std::move(ident)), Type(type) {
    this->IsGlobal = false;
}
bool DeclarationASTnode::getIsGlobal() {
    return IsGlobal;
}
void DeclarationASTnode::setIsGlobal(bool gl) {
    IsGlobal = gl;
}

std::string DeclarationASTnode::getIdent() const {
    return this->Ident;
}

TOKEN_TYPE DeclarationASTnode::getType() const {
    return Type;
}

std::string DeclarationASTnode::to_string(const size_t tab) const {
    std::stringstream ss;
    ss << std::string(tab, '\t') << "[Declaration Node : Identifier : " << Ident
       << " Type: " << misc::VarTypeToStr(Type) << " " << misc::TokToString(Tok)
       << "]";
    return ss.str();
}

//===----------------------------------------------------------------------===//

//===----------------------------------------------------------------------===//
// BodyASTnode
BodyASTnode::BodyASTnode(VectorAST localD, VectorAST stmtL)
    : LocalD(std::move(localD)), StmtL(std::move(stmtL)) {}
void BodyASTnode::setIsMain(bool Val) {
    IsMain = Val;
}

bool BodyASTnode::getIsMain() {
    return IsMain;
}

std::string BodyASTnode::to_string(const size_t tab) const {
    std::stringstream ss;
    ss << std::string(tab, '\t') << "[Body node : Local Declarations : ["
       << std::endl;
    for (const auto& pt : LocalD) {
        ss << pt->to_string(tab + 1) << std::endl;
    }
    ss << std::string(tab, '\t') << "] StatementList : [" << std::endl;
    for (const auto& pt : StmtL) {
        ss << pt->to_string(tab + 1) << std::endl;
    }
    ss << std::string(tab, '\t') << "] ]";
    return ss.str();
}
//===----------------------------------------------------------------------===//

//===----------------------------------------------------------------------===//
// ProgramASTnode
ProgramASTnode::ProgramASTnode(VectorAST externL, VectorAST declL)
    : ExternL(std::move(externL)), DeclL(std::move(declL)) {}

std::string ProgramASTnode::to_string(const size_t tab) const {
    std::stringstream ss;
    ss << std::string(tab, '\t') << "[Program node : ExternList : ["
       << std::endl;
    for (const auto& ex : ExternL) {
        ss << ex->to_string(tab + 1) << std::endl;
    }
    ss << std::string(tab, '\t') << "] DeclarationList : [" << std::endl;
    for (const auto& st : DeclL) {
        ss << st->to_string(tab + 1) << std::endl;
    }
    ss << std::string(tab, '\t') << "]]";
    return ss.str();
}
//===----------------------------------------------------------------------===//

//===----------------------------------------------------------------------===//
// ExternFunctionDeclASTnode
ExternFunctionDeclASTnode::ExternFunctionDeclASTnode(std::string ident,
                                                     TOKEN_TYPE type,
                                                     Args_t args)
    : Ident(std::move(ident)), RetType(type), Args(std::move(args)) {}

std::string ExternFunctionDeclASTnode::to_string(const size_t tab) const {
    std::stringstream ss;
    ss << std::string(tab, '\t') << "[Extern Node : " << misc::TokToString(Tok)
       << " Identifier: " << Ident << " Type: ";
    ss << misc::VarTypeToStr(RetType) << " Args : ";
    if (std::holds_alternative<VectorDeclAST>(Args)) {
        ss << "[" << std::endl;
        for (const auto& arg : std::get<VectorDeclAST>(Args)) {
            ss << arg->to_string(tab + 1) << std::endl;
        }
        ss << std::string(tab, '\t') << "]";
    } else {
        ss << "[void]";
    }
    ss << "]";
    return ss.str();
}
//===----------------------------------------------------------------------===//

//===----------------------------------------------------------------------===//
// FunctionASTnode
FunctionASTnode::FunctionASTnode(Args_t args, UPtrASTnode body,
                                 std::string ident, TOKEN_TYPE ret)
    : Args(std::move(args)), Body(std::move(body)), Ident(std::move(ident)),
      Ret(ret) {}

std::string FunctionASTnode::to_string(const size_t tab) const {
    std::stringstream ss;
    ss << std::string(tab, '\t')
       << "[Function Defintion Node : " << misc::TokToString(Tok)
       << " ReturnType: " << misc::VarTypeToStr(Ret)
       << " Identifier : " << Ident << " Args : ";
    if (std::holds_alternative<VectorDeclAST>(Args)) {
        ss << "[" << std::endl;
        for (const auto& arg : std::get<VectorDeclAST>(Args)) {
            ss << arg->to_string(tab + 1) << ", ";
        }
        ss << std::string(tab, '\t') << "]";
    } else {
        ss << "void]";
    }
    ss << " Body : " << std::endl
       << Body->to_string(tab + 1) << std::endl
       << std::string(tab, '\t') << " ]";
    return ss.str();
}
//===----------------------------------------------------------------------===//

//===----------------------------------------------------------------------===//
// ReturnStatementASTnode
ReturnStatementASTnode::ReturnStatementASTnode(TOKEN tok, OptionalPtr ret_expr)
    : Tok(tok), RetExpr(std::move(ret_expr)){};

std::string ReturnStatementASTnode::to_string(const size_t tab) const {
    std::stringstream ss;
    ss << std::string(tab, '\t') << "[Return Node " << misc::TokToString(Tok);
    if (RetExpr.has_value()) {
        ss << " : Expression :" << std::endl
           << RetExpr.value()->to_string(tab + 1) << std::endl
           << std::string(tab, '\t') << "]";

    } else {
        ss << "]";
    }
    return ss.str();
}
//===----------------------------------------------------------------------===//