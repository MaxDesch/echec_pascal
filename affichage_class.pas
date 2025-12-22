unit affichage_class;
{$mode objfpc}{$H+}
interface

uses
  Classes, SysUtils, SDL2 in 'SDL2-Pascal/units/sdl2.pas',
  SDL2_image in 'SDL2-Pascal/units/sdl2_image.pas',
  SDL2_ttf in 'SDL2-Pascal/units/sdl2_ttf.pas',
  SDL2_gfx in 'SDL2-Pascal/units/sdl2_gfx.pas';
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
function RGB(r, g, b: Byte): TSDL_Color;
function RGBA(r, g, b, a: Byte): TSDL_Color;
function Min(a, b: Byte): Byte;

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
end.