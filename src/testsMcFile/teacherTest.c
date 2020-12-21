int PARAM = 5;

int fact(int n) {
    if (n < 2) {
        return 1;
    } else {
        return n * fact(n + -1);
    }
}

int forFact(int n){
    int prod = 1;
    int i = 1;
    for (;i < n + 1; i = i + 1){
        prod = i * prod;
        //putchar(prod);
    }
    //putchar(prod);
    return prod;
}

void main() {
}
