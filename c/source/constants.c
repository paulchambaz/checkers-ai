#include "constants.h"

u32 get_line(u32 n) {
  return floor((float) n / grid_size);
}

u32 get_col(u32 n) {
  return floor(n % grid_size);
}

u32 get_square(u32 x, u32 y) {
  if (x == -1 || y == -1) { return -1; }
  return y * grid_size + x;
}

void die(char *message) {
  fprintf(stderr, "%s\n", message);
  exit(EXIT_FAILURE);
}
