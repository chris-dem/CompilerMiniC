// MiniC program to compute the fibonachi numbers

extern int print_int(int X);

int fibonacci(int n)
{
  if(n == 0) {
    return 0;
  } 
  if(n == 1) {
    return 1;
  }
  return fibonacci(n - 1) + fibonacci(n - 2);
}