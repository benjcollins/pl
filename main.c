#include <stdio.h>

extern int fib(int n);
extern int sum(int *total, int n);

int main() {
    // int total = 0;
    for (int i = 0; i < 20; i++) {
        int n = fib(i);
        printf("%d\n", n);
        // sum(&total, n);
    }
    // sum(&total, 5);
    // printf("%d\n", total);
}