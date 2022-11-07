#include <deque>
#include <iostream>
#include <list>
#include <optional>
#include <sstream>
#include <string>

int main() {
    std::list<int> li = {1, 2, 3};
    std::deque<int, std::list<int>> st(li);
    for (auto rit = st.rend()) {
    }
    return 0;
}