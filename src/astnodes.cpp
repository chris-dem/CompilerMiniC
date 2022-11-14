#include "astnodes.hpp"
#include "helpers.hpp"
#include <sstream>

//===----------------------------------------------------------------------===//
// ASTnode

ASTnode::~ASTnode() {}

std::string ASTnode::to_string() const {
    return std::string("");
}
//===----------------------------------------------------------------------===//
//===----------------------------------------------------------------------===//
// IntASTnode
IntASTnode::IntASTnode(TOKEN tok, int val) : Tok(tok), Val(val) {}

std::string IntASTnode::to_string() const {
    std::stringstream ss;
    ss << "[Integer literal node : Value : ";
    ss << Val;
    ss << "]";
    return ss.str();
}
//===----------------------------------------------------------------------===//

//===----------------------------------------------------------------------===//
// FloatASTnode
FloatASTnode::FloatASTnode(TOKEN tok, float val) : Tok(tok), Val(val) {}

std::string FloatASTnode::to_string() const {
    std::stringstream ss;
    ss << "[Floating literal node : Value : ";
    ss << Val;
    ss << "]";
    return ss.str();
}
//===----------------------------------------------------------------------===//

//===----------------------------------------------------------------------===//
// BoolASTnode
BoolASTnode::BoolASTnode(TOKEN tok, bool val) : Tok(tok), Val(val) {}

std::string BoolASTnode::to_string() const {
    std::stringstream ss;
    ss << "[Boolean literal node : Value : ";
    ss << Val;
    ss << "]";
    return ss.str();
}
//===----------------------------------------------------------------------===//

//===----------------------------------------------------------------------===//
// IdentifierASTnode
IdentifierASTnode::IdentifierASTnode(TOKEN tok, std::string val)
    : Tok(tok), Val(std::move(val)) {}

std::string IdentifierASTnode::to_string() const {
    std::stringstream ss;
    ss << "[Identifier literal node : Value : ";
    ss << Val;
    ss << "]";
    return ss.str();
}
//===----------------------------------------------------------------------===//

//===----------------------------------------------------------------------===//
// FunctionCallASTnode
FunctionCallASTnode::FunctionCallASTnode(std::string identifier, VectorAST args)
    : Identifier(identifier), Args(std::move(args)) {}

std::string FunctionCallASTnode::to_string() const {
    std::stringstream ss;
    ss << "[FunctionCall literal node : Identifier : " << Identifier
       << " Args: [";
    for (const auto& t : Args) {
        ss << t->to_string() << ", ";
    }
    ss << "] ]";
    return ss.str();
}
//===----------------------------------------------------------------------===////===----------------------------------------------------------------------===//

//===----------------------------------------------------------------------===//
// IdentifierASTnode
BinaryOperatorASTnode::BinaryOperatorASTnode(TOKEN_TYPE op, UPtrASTnode LHS,
                                             UPtrASTnode RHS)
    : Op(op), LHS(std::move(LHS)), RHS(std::move(RHS)) {}

std::string BinaryOperatorASTnode::to_string() const {
    std::stringstream ss;
    ss << "[BinaryOperator literal node : Operator : "
       << misc::convertTokenType(Op) << " LHS : [" << LHS->to_string() << "] "
       << "RHS : [ " << RHS->to_string() << "] ]";
    return ss.str();
}
//===----------------------------------------------------------------------===//

//===----------------------------------------------------------------------===//
// IdentifierASTnode
UnaryOperatorASTnode::UnaryOperatorASTnode(TOKEN_TYPE op, UPtrASTnode expr)
    : Op(op), Expr(std::move(expr)) {}

std::string UnaryOperatorASTnode::to_string() const {
    std::stringstream ss;
    ss << "[Unary literal node : Operator : " << misc::convertTokenType(Op)
       << " Expr : [" << Expr->to_string() << "] ]";
    return ss.str();
}
//===----------------------------------------------------------------------===//

//===----------------------------------------------------------------------===//
// IdentifierASTnode
AssignmentASTnode::AssignmentASTnode(std::string Name, UPtrASTnode rvalue)
    : Name(std::move(Name)), Rvalue(std::move(rvalue)) {}

std::string AssignmentASTnode::to_string() const {
    std::stringstream ss;
    ss << "[Assignement node : Ident : " << Name << " Expr : ["
       << Rvalue->to_string() << "] ]";
    return ss.str();
}
//===----------------------------------------------------------------------===//

//===----------------------------------------------------------------------===//
// IfStatementASTnode
IfStatementASTnode::IfStatementASTnode(UPtrASTnode predicate, UPtrASTnode body,
                                       OptionalPtr else_body)
    : Predicate(std::move(predicate)), Body(std::move(body)),
      Else(std::move(else_body)) {}

std::string IfStatementASTnode::to_string() const {
    std::stringstream ss;
    ss << "[IfStatement node: Predicate : [" << Predicate->to_string()
       << "] Body : [" << Body->to_string() << "] ElseBody : [";
    if (Else.has_value()) {
        ss << Else.value()->to_string();
    } else {
        ss << "No Else Body";
    }
    ss << "] ]";
    return ss.str();
}
//===----------------------------------------------------------------------===//

//===----------------------------------------------------------------------===//
// WhileStatementASTnode

WhileStatementASTnode::WhileStatementASTnode(UPtrASTnode predicate,
                                             UPtrASTnode body)
    : Predicate(std::move(predicate)), Body(std::move(body)) {}

std::string WhileStatementASTnode::to_string() const {
    std::stringstream ss;
    ss << "[WhileStatement node : Predicate : [" << Predicate->to_string()
       << "] Body : [";
    if (Body == nullptr) {
        ss << "Empty Body";
    } else {
        ss << Body->to_string();
    }
    ss << "] ]";
    return ss.str();
}
//===----------------------------------------------------------------------===//

//===----------------------------------------------------------------------===//
// IdentifierASTnode
DeclarationASTnode::DeclarationASTnode(std::string ident, TOKEN_TYPE type)
    : Ident(std::move(ident)), Type(type) {
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

std::string DeclarationASTnode::to_string() const {
    std::stringstream ss;
    ss << "[ Declaration Node : Identifier : " << Ident
       << " Type: " << misc::VarTypeToStr(Type) << "]";
    return ss.str();
}
//===----------------------------------------------------------------------===//

//===----------------------------------------------------------------------===//
// IdentifierASTnode
BodyASTnode::BodyASTnode(VectorAST localD, VectorAST stmtL)
    : LocalD(std::move(localD)), StmtL(std::move(stmtL)) {}
void BodyASTnode::setIsMain(bool Val) {
    IsMain = Val;
}

bool BodyASTnode::getIsMain() {
    return IsMain;
}

std::string BodyASTnode::to_string() const {
    std::stringstream ss;
    ss << "[ Body node : Local Declarations : [";
    for (const auto& pt : LocalD) {
        ss << pt->to_string() << ", ";
    }
    ss << "] StatementList : [";
    for (const auto& pt : StmtL) {
        ss << pt->to_string() << ", ";
    }
    ss << "] ]";
    return ss.str();
}
//===----------------------------------------------------------------------===//

//===----------------------------------------------------------------------===//
// IdentifierASTnode
ProgramASTnode::ProgramASTnode(VectorAST externL, VectorAST declL)
    : ExternL(std::move(externL)), DeclL(std::move(declL)) {}

std::string ProgramASTnode::to_string() const {
    std::stringstream ss;
    ss << "[Program node : \n\tExternList : [";
    for (const auto& ex : ExternL) {
        ss << ex->to_string() << ", ";
    }
    ss << "\n]\nDeclarationList : [";
    for (const auto& st : DeclL) {
        ss << st->to_string() << ", ";
    }
    ss << "\n ]]";
    return ss.str();
}
//===----------------------------------------------------------------------===//

//===----------------------------------------------------------------------===//
// IdentifierASTnode
ExternFunctionDeclASTnode::ExternFunctionDeclASTnode(std::string ident,
                                                     TOKEN_TYPE type,
                                                     Args_t args)
    : Ident(std::move(ident)), RetType(type), Args(std::move(args)) {}

std::string ExternFunctionDeclASTnode::to_string() const {
    std::stringstream ss;
    ss << "[Extern Node : Identifier: " << Ident << " Type: ";
    ss << misc::VarTypeToStr(RetType) << " Args : [";
    if (std::holds_alternative<VectorDeclAST>(Args)) {
        for (const auto& arg : std::get<VectorDeclAST>(Args)) {
            ss << arg->to_string() << ", ";
        }
    } else {
        ss << "void]";
    }
    ss << "] ]";
    return ss.str();
}
//===----------------------------------------------------------------------===//

//===----------------------------------------------------------------------===//
// IdentifierASTnode
FunctionASTnode::FunctionASTnode(Args_t args, UPtrASTnode body,
                                 std::string ident, TOKEN_TYPE ret)
    : Args(std::move(args)), Body(std::move(body)), Ident(std::move(ident)),
      Ret(ret) {}

std::string FunctionASTnode::to_string() const {
    std::stringstream ss;
    ss << "[ Function Defintion Node : ReturnType: " << misc::VarTypeToStr(Ret)
       << " Identifier : " << Ident << " Args : [";
    if (std::holds_alternative<VectorDeclAST>(Args)) {
        for (const auto& arg : std::get<VectorDeclAST>(Args)) {
            ss << arg->to_string() << ", ";
        }
    } else {
        ss << "void]";
    }
    ss << "] Body : [\n" << Body->to_string() << "] ]";
    return ss.str();
}
//===----------------------------------------------------------------------===//

//===----------------------------------------------------------------------===//
// IdentifierASTnode
ReturnStatementASTnode::ReturnStatementASTnode(OptionalPtr ret_expr)
    : RetExpr(std::move(ret_expr)){};

std::string ReturnStatementASTnode::to_string() const {
    std::stringstream ss;
    ss << "[ Return Node ";
    if (RetExpr.has_value()) {
        ss << ": Expression : " << RetExpr.value()->to_string() << " ";
    }
    ss << "]";
    return ss.str();
}
//===----------------------------------------------------------------------===//