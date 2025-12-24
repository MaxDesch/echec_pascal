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


var
  partie : TPartie_echec;
  running: Boolean;
  x,y : Integer;


begin
  InitialiserSDL;
  InitialiserTextures(renderer);
  partie := Initialisation_partie();
  couleur_affichage := BLANC;
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
        SDL_MOUSEBUTTONDOWN :
        begin
          x := event.button.x div taille_case;
          y := event.button.y div taille_case;
          Writeln(x,'   ',y);
          Writeln('piece : ',partie.echiquier.echiquier[7-y][7-x].piece);
          Writeln(Length(partie.echiquier.echiquier[7-y][7-x].coups_autoriser));
          gerer_clique(partie,x,y);

        end;
        SDL_KEYDOWN: 
        begin
          case event.key.keysym.sym of
            SDLK_ESCAPE: running := False;
            SDLK_z:
            begin
              calculer_coup_couleur(partie,BLANC);
              Writeln('ez');
            end;
          end;

        end;
      end;
    end;
    SDL_SetRenderDrawColor(renderer, 200, 0, 0, 255);
    SDL_RenderClear(renderer);
    AfficherPartie(partie,renderer);

    SDL_RenderPresent(renderer);
    SDL_Delay(16);
  end;

  DestroyTextures;
  TTF_CloseFont(font);
  SDL_Quit;
end.