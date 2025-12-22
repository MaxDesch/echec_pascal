Unit affichage_echec;

{$mode objfpc}{$H+}
interface

uses
  Classes, SysUtils, SDL2 in 'SDL2-Pascal/units/sdl2.pas',SDL2_image in 'SDL2-Pascal/units/sdl2_image.pas',
  SDL2_ttf in 'SDL2-Pascal/units/sdl2_ttf.pas',
  SDL2_gfx in 'SDL2-Pascal/units/sdl2_gfx.pas',
  echec_utilitaire;
procedure AfficherEchiquier(echiquier: Techiquier; renderer: PSDL_Renderer);
procedure AfficherPiece(echiquier: Techiquier; x, y: Integer; rect: TSDL_Rect; renderer: PSDL_Renderer);
procedure InitialiserTextures(renderer: PSDL_Renderer);
procedure DestroyTextures;
var
  texture_piece: array[-ROI..ROI] of PSDL_Texture;


implementation

procedure InitialiserTextures(renderer: PSDL_Renderer);
var
  piece: Integer;
begin
  // Initialiser toutes les textures à nil
  for piece := -ROI to ROI do
    texture_piece[piece] := nil;
  texture_piece[PION] := IMG_LoadTexture(renderer, 'image/pieces_echecs/pion_blanc.png');
  texture_piece[-PION] := IMG_LoadTexture(renderer, 'image/pieces_echecs/pion_noir.png');
  texture_piece[TOUR] := IMG_LoadTexture(renderer, 'image/pieces_echecs/tour_blanc.png');
  texture_piece[-TOUR] := IMG_LoadTexture(renderer, 'image/pieces_echecs/tour_noir.png');
  texture_piece[FOU] := IMG_LoadTexture(renderer, 'image/pieces_echecs/fou_blanc.png');
  texture_piece[-FOU] := IMG_LoadTexture(renderer, 'image/pieces_echecs/fou_noir.png');
  texture_piece[CAVALIER] := IMG_LoadTexture(renderer, 'image/pieces_echecs/cheval_blanc.png');
  texture_piece[-CAVALIER] := IMG_LoadTexture(renderer, 'image/pieces_echecs/cheval_noir.png');
  texture_piece[DAME] := IMG_LoadTexture(renderer, 'image/pieces_echecs/reine_blanc.png');
  texture_piece[-DAME] := IMG_LoadTexture(renderer, 'image/pieces_echecs/reine_noir.png');
  texture_piece[ROI] := IMG_LoadTexture(renderer, 'image/pieces_echecs/roi_blanc.png');
  texture_piece[-ROI] := IMG_LoadTexture(renderer, 'image/pieces_echecs/roi_noir.png');
end;

procedure DestroyTextures;
var piece: Integer;
begin
  for piece := -ROI to ROI do
  begin
    if texture_piece[piece] <> nil then
    begin
      SDL_DestroyTexture(texture_piece[piece]);
      texture_piece[piece] := nil;
    end;
  end;
end;


procedure AfficherPiece(echiquier: Techiquier; x, y: Integer; rect: TSDL_Rect; renderer: PSDL_Renderer);
var
  piece: Integer;
begin
  piece := echiquier.echiquier[x, y];
  if piece = VIDE then
    Exit; // Ne rien afficher pour une case vide
  // Charger la texture de la pièce en fonction de sa valeur
  
  SDL_RenderCopy(renderer, texture_piece[piece], nil, @rect);
end;

procedure AfficherEchiquier(echiquier: Techiquier; renderer: PSDL_Renderer);
var
  i, j: Integer;
  rect: TSDL_Rect;
begin
  for i := 0 to 7 do
    for j := 0 to 7 do
    begin
      rect.x := j * 60;
      rect.y := i * 60;
      rect.w := 60;
      rect.h := 60;
      if (i + j) mod 2 = 0 then
        SDL_SetRenderDrawColor(renderer, 240, 217, 181, 255) // Couleur claire
      else
        SDL_SetRenderDrawColor(renderer, 181, 136, 99, 255); // Couleur foncée
      SDL_RenderFillRect(renderer, @rect);
      AfficherPiece(echiquier, i, j, rect, renderer);
    end;
end;


end.