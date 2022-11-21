#include "helpers.hpp"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Type.h"
#include <iostream>
#include <sstream>

bool misc::checkStrictEval(const TOKEN_TYPE& type) {
    switch (type) {
    case TOKEN_TYPE::PLUS:    // +
    case TOKEN_TYPE::ASTERIX: // *
    case TOKEN_TYPE::MINUS:   // -
    case TOKEN_TYPE::DIV:     // /
    case TOKEN_TYPE::MOD:     // %
    case TOKEN_TYPE::EQ:      // ==
    case TOKEN_TYPE::NE:      // !=
    case TOKEN_TYPE::LT:      // <
    case TOKEN_TYPE::LE:      // <=
    case TOKEN_TYPE::GT:      // >
    case TOKEN_TYPE::GE:      // >=
        return true;

    default:
        break;
    }
    return false;
}
bool misc::checkTokenInGroup(const int& type) {
    switch (type) {
    case TOKEN_TYPE::IDENT:
    case TOKEN_TYPE::INT_LIT:
    case TOKEN_TYPE::FLOAT_LIT:
    case TOKEN_TYPE::BOOL_LIT:
    case TOKEN_TYPE::LPAR:
    case TOKEN_TYPE::MINUS:
    case TOKEN_TYPE::NOT:
        return true;
    }
    return false;
}

std::string misc::convertTokenType(const TOKEN_TYPE& t) {
    std::string ret;
    switch (t) {
    case TOKEN_TYPE::AND:
        return "&&";
        break;

    case TOKEN_TYPE::OR:
        return "||";
        break;
    case TOKEN_TYPE::PLUS:
        return "+";
        break;

    case TOKEN_TYPE::MINUS:
        return "-";
        break;

    case TOKEN_TYPE::ASTERIX:
        return "*";
        break;
    case TOKEN_TYPE::DIV:
        return "/";
        break;
    case TOKEN_TYPE::EQ:
        return "==";
        break;

    case TOKEN_TYPE::NE:
        return "!=";
        break;

    case TOKEN_TYPE::MOD:
        return "%";
        break;

    case TOKEN_TYPE::LE:
        return "<=";
        break;

    case TOKEN_TYPE::LT:
        return "<";
        break;

    case TOKEN_TYPE::GE:
        return ">=";
        break;

    case TOKEN_TYPE::GT:
        return ">";
        break;
    case TOKEN_TYPE::NOT:
        return "!";
        break;
    default:
        return "ERROR";
    }

    return "ERROR";
}

bool misc::checkTokenVarType(const int& type) {
    switch (type) {
    case TOKEN_TYPE::INT_TOK:
    case TOKEN_TYPE::BOOL_TOK:
    case TOKEN_TYPE::FLOAT_TOK:
        return true;
    }
    return false;
}

bool misc::checkIdent(const TOKEN& t) {
    return t.type == TOKEN_TYPE::IDENT;
}

bool misc::checkTokInGroup(const TOKEN& tok,
                           const std::vector<TOKEN_TYPE>& arr) {
    for (const auto& tok_t : arr)
        if (tok.type == tok_t)
            return true;
    return false;
}

bool misc::checkTokenRetType(const int& type) {
    switch (type) {
    case TOKEN_TYPE::INT_TOK:
    case TOKEN_TYPE::BOOL_TOK:
    case TOKEN_TYPE::FLOAT_TOK:
    case TOKEN_TYPE::VOID_TOK:
        return true;
    }
    return false;
}

std::string misc::VarTypeToStr(const TOKEN_TYPE& t) {
    std::string st;
    switch (t) {
    case TOKEN_TYPE::INT_TOK:
        st = "int";
        break;
    case TOKEN_TYPE::FLOAT_TOK:
        st = "float";
        break;
    case TOKEN_TYPE::BOOL_TOK:
        st = "bool";
        break;
    case TOKEN_TYPE::VOID_TOK:
        st = "void";
        break;
    default:
        st = "ERROR";
    }
    return st;
}

std::string misc::TokToString(const TOKEN& t) {
    std::stringstream ss;
    ss << "at line " << t.lineNo << " col no " << t.columnNo;

    return ss.str();
}
