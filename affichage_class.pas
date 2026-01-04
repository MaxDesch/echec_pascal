unit affichage_class;
{$mode objfpc}{$H+}
interface

uses
  Classes, SysUtils, SDL2 in 'SDL2-Pascal/units/sdl2.pas',
  SDL2_image in 'SDL2-Pascal/units/sdl2_image.pas',
  SDL2_ttf in 'SDL2-Pascal/units/sdl2_ttf.pas',
  SDL2_gfx in 'SDL2-Pascal/units/sdl2_gfx.pas',
  Math;
type

  TBouton = class
  private
    FRect: TSDL_Rect;
    FColor: TSDL_Color;
    Fclicked: Boolean;
    FHovered: Boolean;
    FHoveredColor: TSDL_Color;
    Ftext: string;
    FColorText: TSDL_Color;
    pointer_font : PTTF_Font;
  public
    constructor Create(AX, AY, AWidth, AHeight: Integer; AColor: TSDL_Color);
    procedure SetText(AText: string; AColor: TSDL_Color; font: PTTF_Font);
    procedure clicked; virtual;
    procedure SetHovered(AHovered: Boolean);
    procedure Draw(ARenderer: PSDL_Renderer);
  end;
  TAffichageScrollable = class
  private
    FEcart : Integer;
    FRectAffichage, FRectTextureToAffichage, FRectSrollBar, FRectBG : TSDL_Rect;
    FCouleurBG : TSDL_Color;
    FSurfaceReel : PSDL_Surface;
    FTextureReel : PSDL_Texture;
    FListeSurface : array of PSDL_Surface;
    procedure AjusterScrollBar;
  public
    constructor Create(AX, AY, AWidth, AHeight, AEcart: Integer; AColor: TSDL_Color);
    procedure Draw(ARenderer: PSDL_Renderer);
    procedure Ajouter_Surface(surface: PSDL_Surface; renderer: PSDL_Renderer);
    procedure detruire;
    procedure Scroll(dir:Integer);
  end;
function RGB(r, g, b: Byte): TSDL_Color;
function RGBA(r, g, b, a: Byte): TSDL_Color;
function Min(a, b: Byte): Byte;
function PointInRect(px, py: Integer; const R: TSDL_Rect): Boolean;


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

function PointInRect(px, py: Integer; const R: TSDL_Rect): Boolean;
begin
  Result := (px >= R.x) and (px < R.x + R.w) and (py >= R.y) and (py < R.y + R.h);
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
constructor TBouton.Create(AX, AY, AWidth, AHeight: Integer; AColor: TSDL_Color);
begin
  FRect.x := AX;
  FRect.y := AY;
  FRect.w := AWidth;
  FRect.h := AHeight;
  FColor := AColor;
  Fclicked := False;
  FHovered := False;
  Ftext := '';
  FColorText := RGB(0, 0, 0);
end;
procedure TBouton.SetText(AText: string; AColor: TSDL_Color; font: PTTF_Font);
begin
  Ftext := AText;
  FColorText := AColor;
  pointer_font := font;
end;
procedure TBouton.SetHovered(AHovered: Boolean);
begin
  FHovered := AHovered;
end;
procedure TBouton.Draw(ARenderer: PSDL_Renderer);
var 
  surf : PSDL_Surface;
  texture : PSDL_Texture;
begin
  if FHovered then// Change color on hover (lighter color)
    SDL_SetRenderDrawColor(ARenderer, Min(FColor.r + 50, 255), Min(FColor.g + 50, 255), Min(FColor.b + 50, 255), FColor.a)
  else
    SDL_SetRenderDrawColor(ARenderer, FColor.r, FColor.g, FColor.b, FColor.a);
  SDL_RenderFillRect(ARenderer, @FRect);
  surf := TTF_RenderText_Solid(pointer_font, PChar(Ftext), FColorText);
  texture := SDL_CreateTextureFromSurface(ARenderer, surf);
  SDL_RenderCopy(ARenderer, texture, nil, @FRect);
  SDL_DestroyTexture(texture);
  SDL_FreeSurface(surf);

end;
procedure TBouton.clicked;
begin
  Fclicked := True;
  Writeln('Button clicked at position (', FRect.x, ', ', FRect.y, ')');
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
end;
procedure TAffichageScrollable.Draw(ARenderer:PSDL_Renderer);
begin
  SDL_SetRenderDrawColor(ARenderer, FCouleurBG.r, FCouleurBG.g, FCouleurBG.b, FCouleurBG.a);
  SDL_RenderFillRect(ARenderer, @FRectBG);
  SDL_RenderCopy(ARenderer, FTextureReel, @FRectTextureToAffichage, @FRectAffichage);
  
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

procedure TAffichageScrollable.Scroll(dir:Integer);
var x,y :Integer;
begin
  SDL_GetMouseState(@x,@y);
  WriteLn(x, '  ', y);
  if not PointInRect(x,y,FRectBG) then Exit;
  WriteLn('ez');
  FRectTextureToAffichage.y += -dir * 10;
  if FRectTextureToAffichage.y > (FSurfaceReel^.h - FRectBG.h) then
    FRectTextureToAffichage.y := FSurfaceReel^.h - FRectBG.h;
  if FRectTextureToAffichage.y < 0 then
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
end.