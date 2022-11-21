#ifndef HELPERS_H
#define HELPERS_H

#include "astnodes.hpp"
#include "llvm/IR/Instructions.h"
#include <list>
#include <map>
#include <string>
#include <variant>

// Miscallenous functuon
namespace misc {
    /// Converrt token type to string
    std::string convertTokenType(const TOKEN_TYPE& t);
    /// error value for std::otpional<Value*> types
    const std::optional<Value*> NULLOPTPTR = std::optional(nullptr);
    /// check if value is truthy => either 1 or 0
    Value* check_truthy(Value* LHS);
    /// @brief  check is value is an error or void type
    /// @param Val value to check
    /// @return
    bool checkValidExprType(llvm::Value* Val);
    /// @brief Check if token is in part of one the tokens
    /// @param tok token to check
    /// @param arr small set of token types to check
    /// Assuming small list, much more efficient than using hashsets
    /// @return
    bool checkTokInGroup(const TOKEN& tok, const std::vector<TOKEN_TYPE>& arr);
    /// @brief Check if value is identifier
    /// @param t token value to check
    /// @return
    bool checkIdent(const TOKEN& t);
    /// @brief Check if token is part of FIRST(expr) set
    /// @param type
    /// @return
    bool checkTokenInGroup(const int& type);
    /// @brief Check if token is part of type tokens for variables
    /// @param type
    /// @return
    bool checkTokenVarType(const int& type);
    /// @brief Check if token is part of type tokens for return types
    /// @param type
    /// @return
    bool checkTokenRetType(const int& type);
    /// @brief Check if token_type opeator is a strict eval operator
    /// @param tok
    /// @return
    bool checkStrictEval(const TOKEN_TYPE& tok);
    /// @brief Convert type to string
    /// @param t
    /// @return
    std::string VarTypeToStr(const TOKEN_TYPE& t);
    /// @brief Push back token and revert curtoken. Acts as to restore back the
    /// tokena and set the CurTok
    /// @param prev
    static void restoreState(const TOKEN& prev);
    /// @brief Cast i1 to i32
    /// @param Val
    /// @return
    llvm::Value* CastToi32(llvm::Value* Val);
    /// @brief Find Elem in local scope
    /// @param Name
    /// @return
    static llvm::AllocaInst* findElemSC(const std::string& Name);
    /// @brief Find element in global scope
    /// @param Name
    /// @return
    static llvm::GlobalVariable* findElemGl(const std::string& Name);
    /// @brief Convert token type to llvm type
    /// @param t
    /// @return
    llvm::Type* convertToType(const TOKEN_TYPE& t);
    /// @brief Convert token to string.
    /// Mainly to convert columnNo and LineNo to string
    /// @param t token to convert
    /// @return
    std::string TokToString(const TOKEN& t);
    /// @brief Creaate an alloca instance based on the helper functionm from the
    /// lectures
    /// @param TheFunction
    /// @param VarName
    /// @param Typ
    /// @return
    static llvm::AllocaInst* CreateEntryBlockAlloca(Function* TheFunction,
                                                    const std::string& VarName,
                                                    llvm::Type* Typ);
}; // namespace misc

// Type level programming, use char return as semi colon type and bool as error
// type
using Semi = char;
using Err  = bool;

// namespace for all parse functions
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
    template <typename T> std::unique_ptr<T> LogErrorT(const char* str);
    static std::unique_ptr<ASTnode> ParseTypeIdentSC();
    static std::unique_ptr<ASTnode> ParseProgram();
    static std::unique_ptr<ASTnode> ParseDecl();
    static std::unique_ptr<ASTnode> ParseExtern();
    static std::unique_ptr<BodyASTnode> ParseBlock();
    static std::unique_ptr<DeclarationASTnode> ParseTypeIdent();
    static std::optional<Args_t> ParseParams();
    static std::optional<UPtrASTnode> ParseElse();
    static std::optional<VectorAST> ParseStmtList();
    static std::optional<VectorAST> ParseLocalDelcs();
    static std::optional<VectorAST> ParseArgs();

    template <typename T> std::optional<T> LogErrorOpt(const char* str);

}; // namespace parse
#endif