#include <stdio.h>

extern void my_main();

int print_int(int n) {
    printf("%d\n", n);
    return 0;
}

int main() {
    my_main();
}