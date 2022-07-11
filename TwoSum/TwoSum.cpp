#include <fstream>
#include <iostream>
#include <set>
#include <vector>

std::set<long> readInput(const std::string &fileName)
{
    std::set<long> s;
    std::ifstream in{ fileName };
    long cur;
    while (in >> cur) {
        s.insert(cur);
    }
    return s;
}

int twoSumOnInput(const std::set<long> &input) {
    const auto N = 10000;
    std::vector<bool> ts(2 * N + 2, false);

    for (auto cur : input) {
        auto lb = input.lower_bound(-N - cur);
        auto ub = input.upper_bound(N - cur);

        if (lb == input.end()) lb = input.begin();
        if (ub == input.end()) ub = input.end();

        for (auto it = lb; it != ub; ++it) {
            if (cur + *it < -N || cur + *it > N) continue;
            if (cur == *it) continue;
            ts[cur + *it + N] = true;
        }
    }

    int nts = 0;
    for (int i = 0; i < ts.size(); ++i) {
        nts += ts[i];
    }

    return nts;
}

int main(int argc, char *argv[]) {
    if (argc < 2) return -1;
    const auto input = readInput(argv[1]);
    std::cout << input.size() << std::endl;
    std::cout << twoSumOnInput(input) << std::endl;
}
