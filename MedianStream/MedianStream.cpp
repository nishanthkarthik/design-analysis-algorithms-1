#include <iostream>
#include <fstream>
#include <vector>
#include <algorithm>
#include <limits>

std::vector<int> readInput(const std::string &fileName) {
    std::ifstream in{ fileName };
    std::vector<int> res;
    int n;
    while (in >> n) res.push_back(n);
    return res;
}

std::ostream &operator<<(std::ostream &out, const std::vector<int> &v) {
    out << '[';
    for (auto e : v) out << e << ' ';
    out << ']';
    return out;
}

int medianStream(const std::vector<int> &v) {
    std::vector<int> maxHeap, minHeap;
    maxHeap.reserve(v.size() / 2);
    minHeap.reserve(v.size() / 2);

    auto pushMax = [&](int e) {
        maxHeap.push_back(e);
        std::push_heap(maxHeap.begin(), maxHeap.end());
    };

    auto pushMin = [&](int e) {
        minHeap.push_back(e);
        std::push_heap(minHeap.begin(), minHeap.end(), std::greater<>{});
    };

    auto median = [&] {
        if (minHeap.empty() && maxHeap.empty()) return std::numeric_limits<int>::min();
        if (minHeap.empty()) return maxHeap.front();
        if (maxHeap.empty()) return minHeap.front();
        if ((minHeap.size() + maxHeap.size()) & 1) {
            return minHeap.size() > maxHeap.size() ? minHeap.front() : maxHeap.front();
        } else {
            return maxHeap.front();
        }
    };

    int medianSum = 0;

    // [[ maxHeap ] -- [ minHeap ]]
    for (int e : v) {
        if (minHeap.empty() || e < minHeap.front()) {
            pushMax(e);
        } else {
            pushMin(e);
        }

        if (maxHeap.size() > 1 && minHeap.size() < maxHeap.size() - 1) {
            int top = maxHeap.front();
            std::pop_heap(maxHeap.begin(), maxHeap.end());
            maxHeap.pop_back();
            pushMin(top);
        }
        if (minHeap.size() > 1 && maxHeap.size() < minHeap.size() - 1) {
            int top = minHeap.front();
            std::pop_heap(minHeap.begin(), minHeap.end(), std::greater<>{});
            minHeap.pop_back();
            pushMax(top);
        }
        medianSum += median();
        medianSum %= 10000;
    }

    return medianSum;
}

int main(int argc, char *argv[]) {
    if (argc < 2) return 1;
    const auto input = readInput(argv[1]);
    std::cout << input.size() << std::endl;
    std::cout << medianStream(input) << std::endl;
}
