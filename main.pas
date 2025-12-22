program echecs_pascal;

{}
{$mode objfpc}{$H+}
uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF WINDOWS}
  Windows,
  {$ENDIF}
  SDL2 in 'SDL2-Pascal/units/sdl2.pas',
  SDL2_image in 'SDL2-Pascal/units/sdl2_image.pas',
  SDL2_ttf in 'SDL2-Pascal/units/sdl2_ttf.pas',
  SDL2_gfx in 'SDL2-Pascal/units/sdl2_gfx.pas',
  affichage_class,
  affichage_echec,
  echec_utilitaire,
  Classes, SysUtils;

const
  SCREEN_WIDTH = 640;
  SCREEN_HEIGHT = 480;

var
  window: PSDL_Window;
  renderer: PSDL_Renderer;
  running: Boolean;
  event: TSDL_Event;
  rect : TSDL_Rect;
  x,y : Integer;
  bouton : TBouton;
  font : PTTF_Font;
  echiquier: Techiquier;


begin
  if SDL_Init(SDL_INIT_VIDEO) <> 0 then
  begin
    Writeln('Unable to initialize SDL: ', SDL_GetError);
    Halt(1);
  end;

  // Your game code would go here
  window := SDL_CreateWindow('Echecs Pascal', SDL_WINDOWPOS_CENTERED, SDL_WINDOWPOS_CENTERED, 800, 600, SDL_WINDOW_SHOWN);
  if window = nil then
  begin
    Writeln('Could not create window: ', SDL_GetError);
    SDL_Quit;
    Halt(1);
  end;
  renderer := SDL_CreateRenderer(window, -1, SDL_RENDERER_ACCELERATED);
  if renderer = nil then
  begin
    Writeln('Could not create renderer: ', SDL_GetError);
    SDL_DestroyWindow(window);
    SDL_Quit;
    Halt(1);
  end;

  if TTF_Init() = -1 then
  begin
    Writeln('Could not initialize TTF: ', TTF_GetError);
    SDL_DestroyRenderer(renderer);
    SDL_DestroyWindow(window);
    SDL_Quit;
    Halt(1);
  end;

  font := TTF_OpenFont('C:/Windows/Fonts/arial.ttf', 50);
  if font = nil then
  begin
    Writeln('Could not load font: ', TTF_GetError);
    SDL_DestroyRenderer(renderer);
    SDL_DestroyWindow(window);
    SDL_Quit;
    Halt(1);
  end;
  InitialiserTextures(renderer);
  echiquier := InitialiserEchiquier();

  running := True;

  while running do
  begin
  if SDL_RenderSetLogicalSize(renderer, SCREEN_WIDTH, SCREEN_HEIGHT) <> 0 then
    Halt;
  while SDL_PollEvent(@event) <> 0 do
    begin
      case event.type_ of
        SDL_QUITEV: running := False;
        SDL_MOUSEMOTION: 
        begin
        end;
        SDL_KEYDOWN: 
        begin
          case event.key.keysym.sym of
            SDLK_ESCAPE: running := False;
            SDLK_a :
            begin
            SDL_GetMouseState(@x, @y);
             Writeln(x, ' ', y);
            SDL_GetWindowSize(window, @x, @y);
            Writeln(x, ' ', y);
            end;
          end;

        end;
      end;
    end;
    SDL_SetRenderDrawColor(renderer, 200, 0, 0, 10);
    SDL_RenderClear(renderer);
    AfficherEchiquier(echiquier, renderer);

    SDL_RenderPresent(renderer);
  end;

  DestroyTextures;
  TTF_CloseFont(font);
  SDL_Quit;
end.