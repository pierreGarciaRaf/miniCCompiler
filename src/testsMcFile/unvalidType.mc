void a(){
    2;
}
int b(int z, int t){
    a();
    3;
}
void main(){
    return b(a(),2);
}