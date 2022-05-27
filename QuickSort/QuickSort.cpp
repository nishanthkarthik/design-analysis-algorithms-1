#include <iostream>
#include <vector>
#include <fstream>
#include <algorithm>
#include <array>

std::ostream &operator<<(std::ostream &out, const std::vector<int> &v) {
    for (auto e : v) out << e << ' ';
    return out;
}

std::vector<int> readInput(std::string fileName) {
    std::ifstream in(fileName);
    std::vector<int> v;
    int n;
    while (in >> n) v.push_back(n);
    return v;
}

struct PivotFirst {
    int operator()(const std::vector<int> &v, int l, int r) { return l; }
};

struct PivotLast {
    int operator()(const std::vector<int> &v, int l, int r) { return r; }
};

struct PivotMedian {
    int operator()(const std::vector<int> &v, int l, int r) {
        int m = (l + r) / 2;
        auto c = std::array<std::pair<int, int>, 3>{
            std::pair<int, int>{l, v[l]},
            std::pair<int, int>{m, v[m]},
            std::pair<int, int>{r, v[r]} };
        std::sort(c.begin(), c.end(), [](auto a, auto b) { return a.second < b.second; });
        return c[1].first;
    }
};

template <typename PivotFn>
int quicksort(std::vector<int> &v, int l, int r) {
    const int window = r - l + 1;
    if (window <= 1) return std::max(0, window - 1);
    const int pivot = PivotFn()(v, l, r);
    if (pivot != l) std::swap(v[pivot], v[l]);
    int feed = l + 1;
    for (int i = l + 1; i <= r; ++i) {
        if (v[i] < v[l]) std::swap(v[feed++], v[i]);
    }
    std::swap(v[l], v[feed - 1]);
    return std::max(0, window - 1) + quicksort<PivotFn>(v, l, feed - 2) + quicksort<PivotFn>(v, feed, r);
}

int main(int argc, char *argv[]) {
    if (argc < 2) return 1;
    const auto input = readInput(argv[1]);

    {
        auto work = input;
        std::cout << "PivotFirst " << quicksort<PivotFirst>(work, 0, work.size() - 1) << std::endl;
        std::cout << "is_sorted " << std::is_sorted(work.begin(), work.end()) << std::endl;
    }

    {
        auto work = input;
        std::cout << "PivotLast " << quicksort<PivotLast>(work, 0, work.size() - 1) << std::endl;
        std::cout << "is_sorted " << std::is_sorted(work.begin(), work.end()) << std::endl;
    }

    {
        auto work = input;
        std::cout << "PivotMedian " << quicksort<PivotMedian>(work, 0, work.size() - 1) << std::endl;
        std::cout << "is_sorted " << std::is_sorted(work.begin(), work.end()) << std::endl;
    }
}
