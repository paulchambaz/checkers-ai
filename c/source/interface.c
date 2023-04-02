#include "interface.h"

/**
 * Clears the SDL renderer to black
 */
void clear(SDL_Renderer *renderer) {
  SDL_SetRenderDrawColor(renderer, 0, 0, 0, 255);
  SDL_RenderClear(renderer);
}

SDL_Rect get_rect(u8 x, u8 y) {
  return (SDL_Rect) {
    floor(x * size + padding + border),
    floor(y * size + padding + border),
    size,
    size
  };
}

/* u8 get_pos_dimension(u32 position, u32 dimension) { */
/*   u32 out = padding + border; */
/*   if (position < out || position >= dimension - out) { */
/*     return -1; */
/*   } */
/*   return floor(grid_size * ((float) (position - out) / (dimension - (2 * out)))); */
/* } */

/* struct position get_pos(u32 mouse_x, u32 mouse_y) { */
/*   return (struct position) { */
/*     get_pos_dimension(mouse_x, width), */
/*     get_pos_dimension(mouse_y, height) */
/*   }; */
/* } */

void draw_checker(SDL_Renderer *renderer) {
  SDL_SetRenderDrawColor(renderer, 25, 25, 25, 255);
  SDL_Rect bg_rect = {0, 0, width, height};
  SDL_RenderFillRect(renderer, &bg_rect);

  SDL_SetRenderDrawColor(renderer, 235, 235, 235, 255);
  SDL_Rect board_rect = {padding, padding, width - 2 * padding, height - 2 * padding};
  SDL_RenderFillRect(renderer, &board_rect);

  SDL_Rect whites[32];
  SDL_Rect blacks[32];
  int white_count = 0;
  int black_count = 0;
  for (int x = 0; x < grid_size; x++) {
    for (int y = 0; y < grid_size; y++) {
      SDL_Rect rect = get_rect(x, y);
      if ((x % 2 == 0 && y % 2 == 1) || (x % 2 == 1 && y % 2 ==0)) {
        whites[white_count++] = rect;
      } else {
        blacks[black_count++] = rect;
      }
    }
  }

  SDL_SetRenderDrawColor(renderer, 235, 235, 235, 255);
  SDL_RenderFillRects(renderer, whites, 32);
  SDL_SetRenderDrawColor(renderer, 25, 25, 25, 255);
  SDL_RenderFillRects(renderer, blacks, 32);
}
