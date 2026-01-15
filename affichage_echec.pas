Unit affichage_echec;

{$mode objfpc}{$H+}
interface

uses
  Classes, SysUtils, SDL2 in 'SDL2-Pascal/units/sdl2.pas',SDL2_image in 'SDL2-Pascal/units/sdl2_image.pas',
  SDL2_ttf in 'SDL2-Pascal/units/sdl2_ttf.pas',
  SDL2_gfx in 'SDL2-Pascal/units/sdl2_gfx.pas',
  echec_utilitaire,
  affichage_class;

procedure AfficherEchiquier(echiquier: Techiquier; renderer: PSDL_Renderer);
procedure AfficherPiece(echiquier: Techiquier; x, y: Integer; rect: TSDL_Rect; renderer: PSDL_Renderer);
procedure InitialiserTextures(renderer: PSDL_Renderer);
procedure DestroyTextures;
procedure InitialiserSDL;
procedure AfficherPartie(partie:TPartie_echec; renderer: PSDL_Renderer);
procedure AfficherInformation(partie:TPartie_echec; renderer: PSDL_Renderer);
procedure initialiserMenuPrincipal;
procedure gerer_event_menu(event: TSDL_Event; var menu: TMenu);
procedure gerer_event_partie(event: TSDL_Event; var partie: TPartie_echec);


const
  SCREEN_WIDTH = 640 ;
  SCREEN_HEIGHT = 360 ;
  taille_case = SCREEN_HEIGHT div 8;

  TIMER_UPDATE = SDL_USEREVENT + 1; 

  // Affichage actuel
  MAINMENU = 0;
  PARTIE_ECHEC = 1;

var
  texture_piece: array[-ROI..ROI] of PSDL_Texture;
  point_deplacement : PSDL_Texture;
  point_capture : PSDL_Texture;
  point_selectionner : PSDL_Texture;
  window: PSDL_Window;
  renderer: PSDL_Renderer;
  event: TSDL_Event;
  x,y : Integer;
  font , font_detailler : PTTF_Font;
  couleur_affichage : Integer;
  actual_screen_width : Integer = 1600;
  actual_screen_heigth : Integer = 900;
  ratioscreen_x : Real ;
  ratioscreen_y : Real ;
  MenuPrincipale : TMenu;
  bouton : TBouton;
  mode_affichage : Integer = MAINMENU;
  
  
implementation

procedure InitialiserSDL;
var icon : PSDL_Surface;
begin
  if SDL_Init(SDL_INIT_VIDEO) <> 0 then
  begin
    Writeln('Unable to initialize SDL: ', SDL_GetError);
    Halt(1);
  end;

  // Your game code would go here
  window := SDL_CreateWindow('Echecs Pascal', SDL_WINDOWPOS_CENTERED, SDL_WINDOWPOS_CENTERED, actual_screen_width, actual_screen_heigth, SDL_WINDOW_SHOWN);
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

  font := TTF_OpenFont('font/gau_font_cube/GAU_cube_B.TTF', 10);
  if font = nil then
  begin
    Writeln('Could not load font: ', TTF_GetError);
    SDL_DestroyRenderer(renderer);
    SDL_DestroyWindow(window);
    SDL_Quit;
    Halt(1);
  end;
  font_detailler := TTF_OpenFont('font/gau_font_cube/GAU_cube_B.TTF', 50);

  icon := IMG_Load('image/pouce_en_air.png');
  SDL_SetWindowIcon(window,icon);
  SDL_FreeSurface(icon);
end;

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
  point_capture := IMG_LoadTexture(renderer, 'image/point/point_rougev2.png');
  point_deplacement := IMG_LoadTexture(renderer, 'image/point/point_grisv2.png');
  point_selectionner := IMG_LoadTexture(renderer, 'image/point/selectione.png');
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
  SDL_DestroyTexture(point_capture);
  SDL_DestroyTexture(point_deplacement);
  SDL_DestroyTexture(point_selectionner);
end;

procedure AfficherPiece(echiquier: Techiquier; x, y: Integer; rect: TSDL_Rect; renderer: PSDL_Renderer);
var
  piece: Integer;
begin
  piece := echiquier.echiquier[x, y].piece;
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
      if couleur_affichage = BLANC then
      begin
        rect.x := (7 - j) * taille_case;
        rect.y := (7 - i) * taille_case;
      end
      else
      begin
        rect.x := j * taille_case;
        rect.y := i * taille_case;
      end;
      rect.w := taille_case;
      rect.h := taille_case;
      if (i + j) mod 2 = 0 then
        SDL_SetRenderDrawColor(renderer, 240, 217, 181, 255) // Couleur claire
      else
        SDL_SetRenderDrawColor(renderer, 181, 136, 99, 255); // Couleur foncée
      SDL_RenderFillRect(renderer, @rect);
      AfficherPiece(echiquier, i, j, rect, renderer);
    end;
end;

procedure AfficherCase(renderer: PSDL_Renderer);
var i, j: Integer;
  rect: TSDL_Rect;
begin
  rect.w := taille_case;
  rect.h := taille_case;
  for i := 0 to 7 do
    for j := 0 to 7 do
    begin
      rect.x := j * taille_case;
      rect.y := i * taille_case;
      if (i + j) mod 2 = 0 then
        SDL_SetRenderDrawColor(renderer, 240, 217, 181, 255) // Couleur claire
      else
        SDL_SetRenderDrawColor(renderer, 181, 136, 99, 255); // Couleur foncée
      SDL_RenderFillRect(renderer, @rect);
    end;
end;

procedure AfficherAllPoint(partie:TPartie_echec;renderer:PSDL_Renderer);
var rect:TSDL_Rect; i : Integer; tab_coup : array of TCoup;
begin
  //check si une piece est selectioner
  if partie.piece_selectione.x = -1 then
    Exit;
  //afficher le point de la piece selectioner
  rect.w := taille_case;
  rect.h := taille_case;
  if partie.couleur_affichage = BLANC then
    begin
      rect.x := (7 - partie.piece_selectione.y) * taille_case;
      rect.y := (7 - partie.piece_selectione.x) * taille_case;
    end
    else
    begin
      rect.x := partie.piece_selectione.y * taille_case;
      rect.y := partie.piece_selectione.x * taille_case;
    end;
  SDL_RenderCopy(renderer, point_selectionner, nil, @rect); 
  //afficher les point de déplacement
  tab_coup := partie.echiquier.echiquier[partie.piece_selectione.x][partie.piece_selectione.y].coups_autoriser;
  for i:= 0 to Length(tab_coup) - 1 do
  begin
    if partie.couleur_affichage = BLANC then
    begin
      rect.x := (7 - tab_Coup[i].yArrivee) * taille_case;
      rect.y := (7 - tab_Coup[i].xArrivee) * taille_case;
    end
    else
    begin
      rect.x := tab_Coup[i].yArrivee * taille_case;
      rect.y := tab_Coup[i].xArrivee * taille_case;
    end;
    if tab_Coup[i].pieceCapturee <> VIDE then
      SDL_RenderCopy(renderer, point_capture, nil, @rect)
    else
      SDL_RenderCopy(renderer, point_deplacement, nil, @rect);
  end;
end;

procedure AfficherAllPiece(partie : TPartie_echec; renderer : PSDL_Renderer);
var
  i, j: Integer;
  rect: TSDL_Rect;
begin
  rect.w := taille_case;
  rect.h := taille_case;
  for i := 0 to 7 do
    for j := 0 to 7 do
    begin
      if partie.echiquier.echiquier[i][j].piece = VIDE then
        continue;
      if partie.couleur_affichage = BLANC then
      begin
        rect.x := (7 - j) * taille_case;
        rect.y := (7 - i) * taille_case;
      end
      else
      begin
        rect.x := j * taille_case;
        rect.y := i * taille_case;
      end;
      AfficherPiece(partie.echiquier, i, j, rect, renderer);
    end;
  
end;

procedure AfficherPartie(partie:TPartie_echec; renderer: PSDL_Renderer);
begin
  if not partie.afficher then
    Exit;
  AfficherEchiquier(partie.echiquier,renderer);
  AfficherCase(renderer);
  AfficherAllPoint(partie,renderer);
  AfficherAllPiece(partie,renderer);
  AfficherInformation(partie,renderer);
  partie.gestionaire.Draw(renderer);
end;

function string_temp(temp:Integer):string;
begin
  if temp > 100 then
  begin
    if ((temp div 10) mod 60) < 10 then 
      Result := IntToStr((temp div 10) div 60) + ':' + IntToStr(0) + IntToStr((temp div 10) mod 60)
    else
      Result := IntToStr((temp div 10) div 60) + ':' + IntToStr((temp div 10) mod 60);
  end
  else 
    Result := IntToStr(temp div 10) + ',' + IntToStr(temp mod 10)
end;

procedure AfficherInformation(partie:TPartie_echec; renderer: PSDL_Renderer);
var rect_text : TSDL_Rect;
  surf : PSDL_Surface;
  texture : PSDL_Texture;
  text : string;
begin
  text := string_temp(partie.timer_blanc);
  rect_text.x := SCREEN_HEIGHT + 30 + (5-Length(text))*17;
  rect_text.y := 30;
  rect_text.w := 70 - (5-Length(text))*17;
  rect_text.h := 25;
  
  surf := TTF_RenderText_Solid(font, PChar(text), RGB(255,255,255));
  texture := SDL_CreateTextureFromSurface(renderer, surf);
  SDL_RenderCopy(renderer, texture, nil, @rect_text);
  SDL_DestroyTexture(texture);
  SDL_FreeSurface(surf);

  text := string_temp(partie.timer_noir);
  rect_text.x := SCREEN_HEIGHT + 170 + (5-Length(text))*17;
  rect_text.y := 30;
  rect_text.w := 70 - (5-Length(text))*17;
  rect_text.h := 25;

  surf := TTF_RenderText_Solid(font, PChar(text), RGB(0,0,0));
  texture := SDL_CreateTextureFromSurface(renderer, surf);
  SDL_RenderCopy(renderer, texture, nil, @rect_text);
  SDL_DestroyTexture(texture);
  SDL_FreeSurface(surf);
end;

procedure initialiserMenuPrincipal;
var couleurBG : TSDL_Color;
begin
  couleurBG := RGB(50,50,50);
  MenuPrincipale := TMenu.Create(couleurBG);
  bouton := TBouton.Create(100,50,SCREEN_WIDTH div 2 - 50, SCREEN_HEIGHT div 2 - 100, RGB(200,200,200), RGB(100,100,100));
  bouton.SetText('Nouvelle Partie', RGB(0,0,0), font_detailler);
  bouton.Set_valeur_modifier(@mode_affichage, PARTIE_ECHEC);
  MenuPrincipale.Ajouter_Bouton(bouton);
end;

procedure gerer_event_partie(event: TSDL_Event; var partie: TPartie_echec);
begin
 case event.type_ of
    SDL_MOUSEMOTION: 
    begin
      partie.gestionaire.gerer_motion(event.motion.yrel) ;
    end;
    SDL_MOUSEWHEEL :
    begin
      partie.gestionaire.Scroll(event.wheel.y);
    end;
    SDL_MOUSEBUTTONDOWN :
    begin
      partie.gestionaire.gerer_clique;
      if event.button.x > SCREEN_HEIGHT then
        Exit;
      x := event.button.x div taille_case;
      y := event.button.y div taille_case;
      gerer_clique(partie, x, y, renderer);
    end;
    SDL_MOUSEBUTTONUP:
      partie.gestionaire.gerer_declique;

    TIMER_UPDATE:
      diminuer_timer(partie);

  end;
end;

procedure gerer_event_menu(event: TSDL_Event; var menu: TMenu);
begin
  case event.type_ of
    SDL_MOUSEMOTION: 
    begin
      menu.gerer_hover(ratioscreen_x,ratioscreen_y);
    end;
    SDL_MOUSEBUTTONDOWN :
    begin
      menu.gerer_clique(ratioscreen_x,ratioscreen_y);
    end;
  end;
end;
end.