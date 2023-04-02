#ifndef CONSTANT_H
#define CONSTANT_H

#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <math.h>
#include <SDL2/SDL.h>

typedef uint8_t u8;
typedef uint32_t u32;
typedef float f32;

static const u32 width = 720;
static const u32 height = width;

static const u32 fps = 60;
static const u32 frame_time = 1000 / fps;

static const u32 padding = 30;
static const u32 border = 5;

static const u32 grid_size = 8;
static const u32 nb_squares = grid_size * grid_size;
static const f32 size = (f32) (width - 2 * (border + padding) - grid_size) / grid_size;

static const u8 white = 0;
static const u8 black = 1;

static const u8 white_pawn = 1;
static const u8 black_pawn = 2;
static const u8 white_king = 3;
static const u8 black_king = 4;

// search depth for minimax
static const u32 search_depth = 32;
// search time for iterative minimax
static const f32 search_time = 10;

struct position {
  u8 x;
  u8 y;
};

u32 get_line(u32 n);
u32 get_col(u32 n);
u32 get_square(u32 x, u32 y);

void die(char *message);

#endif
