#include "constants.h"
#include "interface.h"

int main(int argc, char *argv[]) {

  printf("%f\n", size);

  // initialization of SDL
  SDL_Init(SDL_INIT_VIDEO);
  SDL_Window *window = SDL_CreateWindow( "checkers-ai", SDL_WINDOWPOS_CENTERED, SDL_WINDOWPOS_CENTERED, width, height, SDL_WINDOW_SHOWN);
  if (!window) die("Could not create window");

  SDL_Renderer *renderer = SDL_CreateRenderer(window, -1, 0);
  if (!renderer) die("Could not create renderer");

  SDL_Event event;

  bool quit = false;
  bool mouse_pressed = false;
  u32 frame_start;
  while (!quit) {

    frame_start = SDL_GetTicks();

    while (SDL_PollEvent(&event)) {
      if (event.type == SDL_QUIT) quit = true;
      switch (event.type) {
        case SDL_QUIT:
          quit = true;
          break;
        case SDL_MOUSEBUTTONDOWN:
          if (event.button.button == SDL_BUTTON_LEFT) {
            mouse_pressed = true;
          }
          break;
      }
    }

    clear(renderer);

    draw_checker(renderer);

    SDL_RenderPresent(renderer);

    u32 frame_time_elapsed = SDL_GetTicks() - frame_start;
    if (frame_time_elapsed < frame_time) {
      SDL_Delay(frame_time - frame_time_elapsed);
    }

    mouse_pressed = false;
  }

  SDL_DestroyRenderer(renderer);
  SDL_DestroyWindow(window);
  SDL_Quit();
}
