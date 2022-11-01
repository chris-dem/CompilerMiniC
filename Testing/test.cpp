#include <iostream>
#include <optional>
#include <sstream>
#include <string>

enum COL {
    A = 1,
    B = 2,
};

enum COL2 { C = 1, D = 2 };

struct A_t {
    int x;
};

int main() {
    A_t x = {3};
    A_t y = x;
    x.x++;
    std::cout << x.x << std::endl;
    std::cout << y.x << std::endl;

    return 0;
}