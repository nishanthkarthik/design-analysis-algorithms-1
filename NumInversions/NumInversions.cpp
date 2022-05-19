#include <iostream>
#include <vector>
#include <fstream>
#include <sstream>
#include <cassert>

using Input = std::pair<long, std::vector<int>>;

std::vector<Input> readInputs(const std::string &fileName) {
    std::vector<Input> inputs;
    std::ifstream in{fileName};
    while (in) {
        long invs;
        std::string line;
        std::getline(in, line);
        if (line.empty()) break;
        std::stringstream stream(line);
        std::vector<int> nums;
        stream >> invs;
        while (stream) {
            int num;
            stream >> num;
            nums.push_back(num);
        }
        if (!nums.empty()) nums.pop_back();
        inputs.push_back(Input(invs, std::move(nums)));
    }
    return inputs;
}

std::pair<long, std::vector<int>> mergeSort(const std::vector<int> &in) {
    if (in.size() <= 1) return {0, in};

    int chm = in.size() / 2;
    auto l = mergeSort(std::vector<int>(in.begin(), in.begin() + chm));
    auto r = mergeSort(std::vector<int>(in.begin() + chm, in.end()));
    auto &la = l.second;
    auto &ra = r.second;
    assert(la.size() + ra.size() == in.size());

    long inversions = 0;
    std::vector<int> res;
    int li = 0, ri = 0;
    while (li < la.size() && ri < ra.size()) {
        if (la[li] < ra[ri]) {
            res.push_back(la[li++]);
        } else {
            res.push_back(ra[ri++]);
            inversions += la.size() - li;
        }
    }
    while (li < la.size()) res.push_back(la[li++]);
    while (ri < ra.size()) res.push_back(ra[ri++]);
    return {l.first + r.first + inversions, res};
}

int main() {
    auto inputs = readInputs("NumInversions/tests.txt");
    for (const auto &it: inputs) {
        auto sorted = mergeSort(it.second);
        std::cout << it.first << " ==? " << sorted.first << std::endl;
        assert(sorted.first == it.first);
    }
    std::cout << "Solution "
              << mergeSort(readInputs("NumInversions/IntegerArraySpaced.txt").at(0).second).first
              << std::endl;
    return 0;
}
