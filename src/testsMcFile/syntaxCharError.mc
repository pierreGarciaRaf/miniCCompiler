int PARAM = 5;

int fact(int n) {
    if (n < 2) é {
        return 1;
    } else {
        return n * fact(n + -1);
    }
}

void main() {
    putchar(fact(PARAM));
}
