#include <stdio.h>

extern int fib(int n);
extern int add(int a, int b);

int main() {
    int total = 0;
    for (int i = 0; i < 20; i++) {
        int n = fib(i);
        printf("%d\n", n);
        total = add(total, n);
    }
    printf("%d\n", total);
}