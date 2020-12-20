int PARAM = 5;

int fact(int n) {
    if (n < 2) {
        return 1;
    } else {
        return n * fact(n + -1);
    }
}

void notMain() {
    putchar(fact(PARAM));
}
