#ifndef HELPERS_H
#define HELPERS_H

#include "astnodes.hpp"
#include "llvm/IR/Instructions.h"
#include <list>
#include <map>
#include <string>
#include <variant>

namespace misc {
    std::string convertTokenType(const TOKEN_TYPE& t);
    std::string convertNo(const int& line, const int& col);
    std::string convertNo(const TOKEN& tok);

    const std::optional<Value*> NULLOPTPTR = std::optional(nullptr);

    enum class Var_Type : int {
        Int   = -2,
        Void  = -3,
        Float = -4,
        Bool  = -5,
    };
    bool checkTokInGroup(const TOKEN& tok, const std::vector<TOKEN_TYPE>& arr);
    bool checkIdent(const TOKEN& t);
    bool checkTokenInGroup(const int& type);
    bool checkTokenVarType(const int& type);
    bool checkTokenRetType(const int& type);
    std::string VarTypeToStr(const TOKEN_TYPE& t);
    static void restoreState(const TOKEN& prev);
    static Value* findElem(const std::string& Name);
    void calcBuilder(llvm::Value* val,
                     std::function<void(llvm::Value*)> func_float,
                     std::function<void(llvm::Value*)> func_int);

    static llvm::AllocaInst* CreateEntryBlockAlloca(Function* TheFunction,
                                                    const std::string& VarName,
                                                    llvm::Type* Typ);
}; // namespace misc

using Semi = char;
using Err  = bool;

namespace parse {
    using StmtType = std::variant<std::unique_ptr<ASTnode>, Semi, Err>;

    static std::unique_ptr<ASTnode> ParseIntLit();
    static std::unique_ptr<ASTnode> ParseFloatLit();
    static std::unique_ptr<ASTnode> ParseBoolLit();
    static std::unique_ptr<ASTnode> ParseIdent();
    static std::unique_ptr<ASTnode> ParseFunctionCall();
    static std::unique_ptr<ASTnode> ParseFunction();
    static std::unique_ptr<ASTnode> ParseExpr();
    static std::unique_ptr<ASTnode> ParseRVal();
    static std::unique_ptr<ASTnode> ParseRAnd();
    static std::unique_ptr<ASTnode> ParseREq();
    static std::unique_ptr<ASTnode> ParseRIneq();
    static std::unique_ptr<ASTnode> ParseRPm();
    static std::unique_ptr<ASTnode> ParseRMdm();
    static std::unique_ptr<ASTnode> ParseRp();
    static StmtType ParseStmt();
    static std::unique_ptr<ASTnode> ParseIf();
    static std::unique_ptr<ASTnode> ParseReturn();
    static std::unique_ptr<ASTnode> ParseWhile();
    std::unique_ptr<ASTnode> LogError(const char* str);
    static std::unique_ptr<ASTnode> ParseTypeIdentSC();
    static std::unique_ptr<ASTnode> ParseProgram();
    static std::unique_ptr<ASTnode> ParseDecl();
    static std::unique_ptr<ASTnode> ParseExtern();
    static std::unique_ptr<ASTnode> ParseBlock();
    static std::unique_ptr<ASTnode> ParseTypeIdent();
    static std::optional<Args_t> ParseParams();
    static std::optional<UPtrASTnode> ParseElse();
    static std::optional<VectorAST> ParseStmtList();
    static std::optional<VectorAST> ParseLocalDelcs();
    static std::optional<VectorAST> ParseArgs();

    template <typename T> std::optional<T> LogErrorOpt(const char* str);

}; // namespace parse
#endif