#include <iostream>
#include <stack>
#include <vector>
using namespace std;

// Partition function
int partition(vector<int>& arr, int low, int high) {
    int pivot = arr[high];
    int i = low - 1;
    for (int j = low; j < high; j++) {
        if (arr[j] < pivot) {
            i++;
            swap(arr[i], arr[j]);
        }
    }
    swap(arr[i + 1], arr[high]);
    return i + 1;
}

// quicksort
void quicksort(vector<int>& arr) {
    stack<pair<int, int>> stack;
    int low = 0, high = arr.size() - 1;
    stack.push({low, high});

    while (!stack.empty()) {
        auto [l, h] = stack.top();
        stack.pop();
        int p = partition(arr, l, h);

        if (p - 1 > l) stack.push({l, p - 1}); 
        if (p + 1 < h) stack.push({p + 1, h}); 
    }
}

int main() {
    vector<int> arr = {150, 2, 10, 655, 25, 55};
    quicksort(arr);
    for (int x : arr) cout << x << " ";
    return 0;
}