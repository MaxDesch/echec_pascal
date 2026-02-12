unit affichage_class;
{$mode objfpc}{$H+}

// fichier de toute mes class gérant l'Affichage plus facilement ez
interface

uses
  Classes, SysUtils, SDL2 in 'SDL2-Pascal/units/sdl2.pas',
  SDL2_image in 'SDL2-Pascal/units/sdl2_image.pas',
  SDL2_ttf in 'SDL2-Pascal/units/sdl2_ttf.pas',
  SDL2_gfx in 'SDL2-Pascal/units/sdl2_gfx.pas',
  Math;
type

  rect_color = record
    rect : TSDL_Rect;
    color : TSDL_Color;
  end;

  PInt = ^Integer;

  TBouton = class
  private
    FRect, FRectText: TSDL_Rect;
    FColor: TSDL_Color;
    Fclicked: Boolean;
    FHovered: Boolean;
    FHoveredColor: TSDL_Color;
    Ftext: string;
    FColorText: TSDL_Color;
    pointer_font : PTTF_Font;
    Fpointer_valeur_modifier : array of PInt;
    Fvaleur_modifier : array of Integer;
  public
    constructor Create(AX, AY, AWidth, AHeight: Integer; AColor, AHoveredColor: TSDL_Color);
    procedure SetText(AText: string; AColor: TSDL_Color; font: PTTF_Font);
    procedure Set_valeur_modifier(Aptr : PInt; Avaleur_modifier : Integer);
    procedure gerer_clique(dx,dy: Real);
    procedure gerer_hover(dx,dy: Real);
    procedure Draw(ARenderer: PSDL_Renderer);
  end;


  TAffichageScrollable = class
  private
    FEcart : Integer;
    FRectAffichage, FRectTextureToAffichage, FRectSrollBar, FRectBG : TSDL_Rect;
    FCouleurBG : TSDL_Color;
    FSurfaceReel : PSDL_Surface;
    FTextureReel : PSDL_Texture;
    FRectSrollBarBG : TSDL_Rect;
    FListeSurface : array of PSDL_Surface;
    drag : Boolean;
    procedure AjusterScrollBar;
  public
    constructor Create(AX, AY, AWidth, AHeight, AEcart: Integer; AColor: TSDL_Color);
    procedure Draw(ARenderer: PSDL_Renderer);
    procedure Ajouter_Surface(surface: PSDL_Surface; renderer: PSDL_Renderer);
    procedure detruire;
    procedure Scroll(dir:Integer; dx,dy : Real);
    procedure gerer_clique(x,y:Integer; dx,dy: Real);
    procedure gerer_declique;
    procedure gerer_motion(y : Integer);
    procedure vider(renderer: PSDL_Renderer);
  end;

  PAffichageScrollable = ^TAffichageScrollable ;
  PReal = ^Real;

  TGestionnaireTAffichageScrollable = class
  private
    tab_Affichage : array of PAffichageScrollable;
    dx,dy : PReal;
    utilisedxdy : Boolean;
  public
    constructor Create;
    procedure Definir_Scalaire(x,y : PReal);
    procedure Ajout_Affichage(affichache : PAffichageScrollable);
    procedure Scroll(dir:Integer);
    procedure Draw(ARenderer: PSDL_Renderer);
    procedure gerer_clique;
    procedure gerer_declique;
    procedure gerer_motion(y : Integer);
    procedure detruire;
  end;

  TAffichageScrollableCliquable = class(TAffichageScrollable)
  private
    lst_mot_cliquer : array of String;
    mot_cliquer_index : Integer;
  public
    constructor Create(AX, AY, AWidth, AHeight, AEcart: Integer; AColor: TSDL_Color);
    procedure Ajouter_Surface(surface: PSDL_Surface; renderer: PSDL_Renderer; nom_clique : String) ;
    procedure gerer_clique(dy : Real);
  end;

  TMenu = class
  private
    FCouleurBG : TSDL_Color;
    FButtons : array of TBouton;
    FRect : array of rect_color;
  public
    constructor Create(AColorBG: TSDL_Color);
    procedure Ajouter_Bouton(bouton: TBouton);
    procedure Ajouter_Rect(r: rect_color);
    procedure Draw(ARenderer: PSDL_Renderer);
    procedure gerer_clique(dx,dy: Real);
    procedure gerer_hover(dx,dy: Real);
  end;

  TMenuDeSelectionPiece = class
  private
    FCouleurBG : TSDL_Color;
    FButtons : array of TBouton;
  public
    constructor Create(AColorBG: TSDL_Color);
    procedure Ajouter_Bouton(bouton: TBouton);
    procedure Draw(ARenderer: PSDL_Renderer);
    procedure gerer_clique(dx,dy: Real);
    procedure gerer_hover(dx,dy: Real);
  end;

function RGB(r, g, b: Byte): TSDL_Color;
function RGBA(r, g, b, a: Byte): TSDL_Color;
function Min(a, b: Byte): Byte;
function PointInRect(px, py: Integer; R: TSDL_Rect): Boolean;
function PointInRectScalaire(px, py: Integer; dx, dy : Real; R: TSDL_Rect): Boolean;

implementation

function Min(a, b: Byte): Byte;
begin
  if a < b then
    Result := a
  else
    Result := b;
end;

// Créer une couleur RGB (sans transparence)
function RGB(r, g, b: Byte): TSDL_Color;
begin
  Result.r := r;
  Result.g := g;
  Result.b := b;
  Result.a := 255;
end;

// Créer une couleur RGBA (avec transparence)
function RGBA(r, g, b, a: Byte): TSDL_Color;
begin
  Result.r := r;
  Result.g := g;
  Result.b := b;
  Result.a := a;
end;

function PointInRect(px, py: Integer; R: TSDL_Rect): Boolean;
begin
  Result := (px >= R.x) and (px < R.x + R.w) and (py >= R.y) and (py < R.y + R.h);
end;

function PointInRectScalaire(px, py: Integer; dx, dy : Real; R: TSDL_Rect): Boolean;
begin
  Result := (px * dx >= R.x ) and (px * dx < R.x + R.w) and (py * dy >= R.y) and (py * dy < R.y + R.h);
end;

function ExtendSurface(
  original: PSDL_Surface; 
  addWidth, addHeight: Integer; 
  r, g, b: Byte  // Couleur de fond
): PSDL_Surface;
var
  newSurface: PSDL_Surface;
  newWidth, newHeight: Integer;
  sourceRect, destRect, fullRect: TSDL_Rect;
  bgColor: UInt32;
begin
  if original = nil then Exit(nil);
  
  // Calculer nouvelle taille
  newWidth := original^.w + addWidth;
  newHeight := original^.h + addHeight;
  
  // Vérifier taille minimale
  if (newWidth <= 0) or (newHeight <= 0) then
  begin
    WriteLn('Erreur ExtendSurface: taille invalide');
    Exit(nil);
  end;
  
  // Créer nouvelle surface
  newSurface := SDL_CreateRGBSurface(0, newWidth, newHeight, 32,
    $000000FF, $0000FF00, $00FF0000, $FF000000);
  
  if newSurface = nil then
  begin
    WriteLn('Erreur ExtendSurface: création impossible');
    Exit(nil);
  end;
  
  // Remplir avec la couleur de fond
  bgColor := SDL_MapRGB(newSurface^.format, r, g, b);
  fullRect.x := 0;
  fullRect.y := 0;
  fullRect.w := newWidth;
  fullRect.h := newHeight;
  SDL_FillRect(newSurface, @fullRect, bgColor);
  
  // Calculer la zone à copier
  sourceRect.x := 0;
  sourceRect.y := 0;
  
  // Si on coupe (valeurs négatives), réduire la zone source
  if addWidth < 0 then
    sourceRect.w := original^.w + addWidth
  else
    sourceRect.w := original^.w;
  
  if addHeight < 0 then
    sourceRect.h := original^.h + addHeight
  else
    sourceRect.h := original^.h;
  
  // Position de destination (en haut à gauche)
  destRect.x := 0;
  destRect.y := 0;
  
  // Copier l'originale (ou sa partie)
  SDL_BlitSurface(original, @sourceRect, newSurface, @destRect);
  
  Result := newSurface;
end;

// ===== VERSION ALTERNATIVE: avec SDL_Color =====
function ExtendSurfaceColor(
  original: PSDL_Surface; 
  addWidth, addHeight: Integer; 
  bgColor: TSDL_Color
): PSDL_Surface;
begin
  Result := ExtendSurface(original, addWidth, addHeight, 
    bgColor.r, bgColor.g, bgColor.b);
end;

{ TBouton }
constructor TBouton.Create(AX, AY, AWidth, AHeight: Integer; AColor, AHoveredColor: TSDL_Color);
begin
  FRect.x := AX;
  FRect.y := AY;
  FRect.w := AWidth;
  FRect.h := AHeight;
  FRectText.x := AX + Floor(AWidth * 0.1);
  FRectText.y := AY + Floor(AHeight * 0.1);
  FRectText.w := Floor(AWidth * 0.8);
  FRectText.h := Floor(AHeight * 0.8);
  FColor := AColor;
  Fclicked := False;
  FHovered := False;
  Ftext := '';
  FColorText := RGB(0, 0, 0);
  FHoveredColor := AHoveredColor;
end;
procedure TBouton.SetText(AText: string; AColor: TSDL_Color; font: PTTF_Font);
begin
  Ftext := AText;
  FColorText := AColor;
  pointer_font := font;
end;

procedure TBouton.gerer_hover(dx,dy: Real);
var x,y : Integer;
begin
  SDL_GetMouseState(@x,@y);
  FHovered := PointInRectScalaire(x,y,dx,dy,FRect);
end;

procedure TBouton.Draw(ARenderer: PSDL_Renderer);
var 
  surf : PSDL_Surface;
  texture : PSDL_Texture;
begin
  if FHovered then// Change color on hover (lighter color)
    SDL_SetRenderDrawColor(ARenderer, FHoveredColor.r, FHoveredColor.g, FHoveredColor.b, FHoveredColor.a)
  else
    SDL_SetRenderDrawColor(ARenderer, FColor.r, FColor.g, FColor.b, FColor.a);
  SDL_RenderFillRect(ARenderer, @FRect);
  surf := TTF_RenderText_Solid(pointer_font, PChar(Ftext), FColorText);
  texture := SDL_CreateTextureFromSurface(ARenderer, surf);
  SDL_RenderCopy(ARenderer, texture, nil, @FRectText);
  SDL_DestroyTexture(texture);
  SDL_FreeSurface(surf);

end;

procedure TBouton.Set_valeur_modifier(Aptr : PInt; Avaleur_modifier : Integer);
begin
  SetLength(Fpointer_valeur_modifier, Length(Fpointer_valeur_modifier) + 1);
  SetLength(Fvaleur_modifier, Length(Fvaleur_modifier) + 1);
  Fpointer_valeur_modifier[Length(Fpointer_valeur_modifier) - 1] := Aptr;
  Fvaleur_modifier[Length(Fvaleur_modifier) - 1] := Avaleur_modifier;
end;

procedure TBouton.gerer_clique(dx,dy : Real);
var x,y, i : Integer;
begin
  SDL_GetMouseState(@x,@y);
  if PointInRectScalaire(x,y,dx,dy,FRect) then
  begin 
    if Fpointer_valeur_modifier <> nil then
      for i := 0 to Length(Fpointer_valeur_modifier) - 1 do
        Fpointer_valeur_modifier[i]^ := Fvaleur_modifier[i];
  end;
end;

constructor TAffichageScrollable.Create(AX, AY, AWidth, AHeight, AEcart: Integer; AColor: TSDL_Color);
begin
  FEcart := AEcart;
  FRectAffichage.x := AX;
  FRectAffichage.y := AY;
  FRectAffichage.w := AWidth;
  FRectAffichage.h := 1;
  FCouleurBG := AColor;
  FRectSrollBar.x := AX + AWidth - (AWidth div 12);
  FRectSrollBar.y := AY;
  FRectSrollBar.w := AWidth div 12;
  FRectSrollBar.h := AHeight;
  FRectSrollBarBG.x := AX + AWidth - (AWidth div 12);
  FRectSrollBarBG.y := AY;
  FRectSrollBarBG.w := AWidth div 12;
  FRectSrollBarBG.h := AHeight;
  FRectTextureToAffichage.x := 0;
  FRectTextureToAffichage.y := 0;
  FRectTextureToAffichage.w := AWidth;
  FRectTextureToAffichage.h := AHeight;
  FRectBG.x := AX;
  FRectBG.y := AY;
  FRectBG.w := AWidth;
  FRectBG.h := AHeight;
  FSurfaceReel := SDL_CreateRGBSurface(0, AWidth, 1, 32, $000000FF, $0000FF00, $00FF0000, $FF000000);
  FListeSurface := []; 
  drag := False;
end;
procedure TAffichageScrollable.Draw(ARenderer:PSDL_Renderer);
begin
  SDL_SetRenderDrawColor(ARenderer, FCouleurBG.r, FCouleurBG.g, FCouleurBG.b, FCouleurBG.a);
  SDL_RenderFillRect(ARenderer, @FRectBG);
  SDL_RenderCopy(ARenderer, FTextureReel, @FRectTextureToAffichage, @FRectAffichage);
  
  SDL_SetRenderDrawColor(ARenderer, 100, 100, 100, 255);
  SDL_RenderFillRect(ARenderer, @FRectSrollBarBG);

  SDL_SetRenderDrawColor(ARenderer, 150, 150, 150, 255);
  SDL_RenderFillRect(ARenderer, @FRectSrollBar);
end;

procedure TAffichageScrollable.AjusterScrollBar;
begin
  if FSurfaceReel^.h <= FRectBG.h then
    FRectSrollBar.h := FRectBG.h
  else
    FRectSrollBar.h := Trunc((FRectBG.h / FSurfaceReel^.h) * FRectBG.h);
  FRectSrollBar.y := Trunc(FRectTextureToAffichage.y*(FRectAffichage.h / FSurfaceReel^.h)) + FRectAffichage.y;
end;

procedure TAffichageScrollable.Ajouter_Surface(surface: PSDL_Surface; renderer: PSDL_Renderer);
var newSurf: PSDL_Surface; rect: TSDL_Rect;
begin
  SetLength(FListeSurface,Length(FListeSurface)+1);
  FListeSurface[Length(FListeSurface)-1] := surface;
  rect.x := 0;
  rect.y := FSurfaceReel^.h;
  newSurf := ExtendSurfaceColor(FSurfaceReel, 0, surface^.h + FEcart, FCouleurBG);
  SDL_FreeSurface(FSurfaceReel);
  FSurfaceReel := newSurf;
  if FSurfaceReel^.h <= FRectBG.h then
    FRectAffichage.h := FSurfaceReel^.h
  else
    FRectAffichage.h := FRectBG.h;
  SDL_BlitSurface(surface,nil,FSurfaceReel,@rect);
  SDL_DestroyTexture(FTextureReel);
  FTextureReel := SDL_CreateTextureFromSurface(renderer, FSurfaceReel);
  self.AjusterScrollBar;
  WriteLn(FRectSrollBar.h, '    ', FSurfaceReel^.h);
end;

procedure TAffichageScrollable.Scroll(dir:Integer; dx,dy : Real);
var x,y :Integer;
begin
  SDL_GetMouseState(@x,@y);
  if not PointInRectScalaire(x,y,dx,dy,FRectBG) then Exit;
  FRectTextureToAffichage.y += -dir * 10;
  if FRectTextureToAffichage.y > (FSurfaceReel^.h - FRectBG.h) then
    FRectTextureToAffichage.y := FSurfaceReel^.h - FRectBG.h;
  if FRectTextureToAffichage.y < 0 then
    FRectTextureToAffichage.y := 0;
  self.AjusterScrollBar;
  
end;

procedure TAffichageScrollable.gerer_clique(x,y:Integer; dx,dy: Real);
begin
  if not PointInRectScalaire(x,y,dx,dy,FRectSrollBar) then 
  begin
    drag := False;
    Exit;
  end;
  drag := True;
end;
procedure TAffichageScrollable.gerer_declique;
begin
  drag := False;
end;

procedure TAffichageScrollable.gerer_motion(y : Integer);
begin
  if not drag then Exit;
  FRectTextureToAffichage.y += Ceil(y  * FSurfaceReel^.h/FRectBG.h);
  if FRectTextureToAffichage.y > (FSurfaceReel^.h - FRectBG.h) then
    FRectTextureToAffichage.y := FSurfaceReel^.h - FRectBG.h;
  if FRectTextureToAffichage.y < 0 then
    FRectTextureToAffichage.y := 0;
  self.AjusterScrollBar;
end;

procedure TAffichageScrollable.vider(renderer: PSDL_Renderer);
var i : Integer;
begin
  for i := 0 to Length(FListeSurface) -1 do
    SDL_FreeSurface(FListeSurface[i]);
  SetLength(FListeSurface,0);
  SDL_FreeSurface(FSurfaceReel);
  FSurfaceReel := SDL_CreateRGBSurface(0, FRectBG.w, 1, 32, $000000FF, $0000FF00, $00FF0000, $FF000000);
  SDL_DestroyTexture(FTextureReel);
  FTextureReel := SDL_CreateTextureFromSurface(renderer, FSurfaceReel);
  FRectTextureToAffichage.y := 0;
  self.AjusterScrollBar;
end;

procedure TAffichageScrollable.detruire;
var i : Integer;
begin
  SDL_FreeSurface(FSurfaceReel);
  SDL_DestroyTexture(FTextureReel);
  for i := 0 to Length(FListeSurface) -1 do
    SDL_FreeSurface(FListeSurface[i]);
end;



// TGestionnaireTAffichageScrollable definition
constructor TGestionnaireTAffichageScrollable.Create();
begin
  utilisedxdy := False;
  tab_Affichage := [];
end;

procedure TGestionnaireTAffichageScrollable.Definir_Scalaire(x,y : PReal);
begin
  dx := x;
  dy := y;
  utilisedxdy := True;
end;
procedure TGestionnaireTAffichageScrollable.Ajout_Affichage(affichache : PAffichageScrollable);
begin
  SetLength(tab_Affichage,Length(tab_Affichage)+1);
  tab_Affichage[Length(tab_Affichage)-1] := affichache;
end;

procedure TGestionnaireTAffichageScrollable.Scroll(dir:Integer);
var i : Integer; 
begin
  if utilisedxdy then
  begin
    for i := 0 to Length(tab_Affichage)-1 do
      tab_Affichage[i]^.Scroll(dir,dx^,dy^);
  Exit;
  end;

  for i := 0 to Length(tab_Affichage)-1 do
    tab_Affichage[i]^.Scroll(dir,1,1);
end;


procedure TGestionnaireTAffichageScrollable.Draw(ARenderer:PSDL_Renderer);
var i : Integer; 
begin
  for i := 0 to Length(tab_Affichage)-1 do
    tab_Affichage[i]^.Draw(ARenderer);
end;

procedure TGestionnaireTAffichageScrollable.gerer_clique;
var i, x, y : Integer; 
begin
  SDL_GetMouseState(@x,@y);
  for i := 0 to Length(tab_Affichage)-1 do
    tab_Affichage[i]^.gerer_clique(x,y,dx^,dy^);
end;
procedure TGestionnaireTAffichageScrollable.gerer_declique;
var i : Integer; 
begin
  for i := 0 to Length(tab_Affichage)-1 do
    tab_Affichage[i]^.gerer_declique;
end;
procedure TGestionnaireTAffichageScrollable.gerer_motion(y : Integer);
var i : Integer; 
begin
  for i := 0 to Length(tab_Affichage)-1 do
    tab_Affichage[i]^.gerer_motion(y);
end;

procedure TGestionnaireTAffichageScrollable.detruire;
var i : Integer; 
begin
  for i := 0 to Length(tab_Affichage)-1 do
    tab_Affichage[i]^.detruire;
end;

{ TAffichageScrollableCliquable }

constructor TAffichageScrollableCliquable.Create(AX, AY, AWidth, AHeight, AEcart: Integer; AColor: TSDL_Color);
begin
  inherited Create(AX, AY, AWidth, AHeight, AEcart, AColor);
  lst_mot_cliquer := [];
  mot_cliquer_index := -1;
end;

procedure TAffichageScrollableCliquable.Ajouter_Surface(surface: PSDL_Surface; renderer: PSDL_Renderer; nom_clique : String);
begin
  inherited Ajouter_Surface(surface, renderer);
  SetLength(lst_mot_cliquer, Length(lst_mot_cliquer) + 1);
  lst_mot_cliquer[Length(lst_mot_cliquer) - 1] := nom_clique;
end;

procedure TAffichageScrollableCliquable.gerer_clique(dy : Real);
var i, hauteur_act , x, y : Integer;
begin
  hauteur_act := 0;
  SDL_GetMouseState(@x,@y);
  for i := 0 to Length(FListeSurface) -1 do
  begin
    if (FRectTextureToAffichage.y <= hauteur_act + FListeSurface[i]^.h) and
       (FRectTextureToAffichage.y >= hauteur_act) then
      if (FRectAffichage.y <= y * dy) and
          (FRectAffichage.y + FRectAffichage.h >= y) then
      begin
        mot_cliquer_index := i;
        Exit;
      end;
    hauteur_act += FListeSurface[i]^.h + FEcart;
  end;
end;



{ TMenu }

constructor TMenu.Create(AColorBG: TSDL_Color);
begin
  FCouleurBG := AColorBG;
end;

procedure TMenu.Ajouter_Bouton(bouton: TBouton);
begin
  SetLength(FButtons, Length(FButtons) + 1);
  FButtons[Length(FButtons) - 1] := bouton;
end;

procedure TMenu.Ajouter_Rect(r: rect_color);
begin
  SetLength(FRect, Length(FRect) + 1);
  FRect[Length(FRect) - 1] := r;
end;

procedure TMenu.Draw(ARenderer: PSDL_Renderer);
var i : Integer;
begin
  if FCouleurBG.a <> 0 then
  begin
    SDL_SetRenderDrawColor(ARenderer, FCouleurBG.r, FCouleurBG.g, FCouleurBG.b, FCouleurBG.a);
    SDL_RenderClear(ARenderer);
  end;
  for i := 0 to Length(FRect) - 1 do
  begin
    SDL_SetRenderDrawColor(ARenderer, FRect[i].color.r, FRect[i].color.g, FRect[i].color.b, FRect[i].color.a);
    SDL_RenderFillRect(ARenderer, @FRect[i].rect);
  end;
  for i := 0 to Length(FButtons) - 1 do
    FButtons[i].Draw(ARenderer);
end; 

procedure TMenu.gerer_clique(dx,dy: Real);
var i : Integer;
begin
  for i := 0 to Length(FButtons) -1 do
    FButtons[i].gerer_clique(dx,dy);
end;

procedure TMenu.gerer_hover(dx,dy: Real);
var i : Integer;
begin
  for i := 0 to Length(FButtons) -1 do
    FButtons[i].gerer_hover(dx,dy);
end;

{ TMenuDeSelectionPiece }

constructor TMenuDeSelectionPiece.Create(AColorBG: TSDL_Color);
var i : Integer; bouton : TBouton;
begin
  FCouleurBG := AColorBG;
  FButtons := [];
  for i := 0 to 3 do
  begin
    bouton := TBouton.Create(50, 100 + i*50, 200, 60, RGB(200,200,200), RGB(220,220,220));
    case i of
      0 : bouton.SetText('Dame', RGB(0,0,0), nil);
      1 : bouton.SetText('Tour', RGB(0,0,0), nil);
      2 : bouton.SetText('Fou', RGB(0,0,0), nil);
      3 : bouton.SetText('Cavalier', RGB(0,0,0), nil);
    end;

    self.Ajouter_Bouton(bouton);
  end;
end;

procedure TMenuDeSelectionPiece.Ajouter_Bouton(bouton: TBouton);
begin
  SetLength(FButtons, Length(FButtons) + 1);
  FButtons[Length(FButtons) - 1] := bouton;
end;

procedure TMenuDeSelectionPiece.Draw(ARenderer: PSDL_Renderer);
var i : Integer;
begin
  SDL_SetRenderDrawColor(ARenderer, FCouleurBG.r, FCouleurBG.g, FCouleurBG.b, FCouleurBG.a);
  SDL_RenderClear(ARenderer);
  for i := 0 to Length(FButtons) - 1 do
    FButtons[i].Draw(ARenderer);
end;

procedure TMenuDeSelectionPiece.gerer_clique(dx,dy: Real);
var i : Integer;
begin
  for i := 0 to Length(FButtons) -1 do
    FButtons[i].gerer_clique(dx,dy);
end;

procedure TMenuDeSelectionPiece.gerer_hover(dx,dy: Real);
var i : Integer;
begin
  for i := 0 to Length(FButtons) -1 do
    FButtons[i].gerer_hover(dx,dy);
end;

end.