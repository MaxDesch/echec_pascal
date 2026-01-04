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
  TIMER_UPDATE = SDL_USEREVENT + 1; 

var
  partie : TPartie_echec;
  running: Boolean;
  x,y : Integer;
  timerID :TSDL_TimerID;
  affichagescrollable : TAffichageScrollable;

function TimerCallback(interval: UInt32; param: Pointer): UInt32; cdecl;
var
  userEvent: TSDL_Event;
begin
  // Créer un événement personnalisé
  FillChar(userEvent, SizeOf(userEvent), 0);
  userEvent.type_ := TIMER_UPDATE;
  userEvent.user.code := 0;
  userEvent.user.data1 := nil;
  userEvent.user.data2 := nil;
  
  // Pousser l'événement dans la file d'événements SDL
  SDL_PushEvent(@userEvent);
  
  // Retourner l'intervalle pour que le timer continue
  Result := interval;
end;

begin
  InitialiserSDL;
  InitialiserTextures(renderer);
  partie := Initialisation_partie();
  timerID := SDL_AddTimer(100, @TimerCallback, nil);
  affichagescrollable := TAffichageScrollable.Create(390, 60, 100, 200, 5, RGB(0,0,0));
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
          {WriteLn(event.motion.yrel)}
        end;
        SDL_MOUSEWHEEL :
        begin
        affichagescrollable.Scroll(event.wheel.y);
        end;
        SDL_MOUSEBUTTONDOWN :
        begin
          if event.button.x > SCREEN_HEIGHT then
            continue;
          x := event.button.x div taille_case;
          y := event.button.y div taille_case;
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
            SDLK_w:
            begin
              affichagescrollable.Ajouter_Surface(TTF_RenderText_Solid(font, 'ez doigby', RGB(255,255,255)),renderer);
            end;
          end;
        end;
        TIMER_UPDATE:
          begin
            // L'événement timer s'est déclenché !
            diminuer_timer(partie);
          end;
      end;
    end;
    SDL_SetRenderDrawColor(renderer, 125, 125, 125, 255);
    SDL_RenderClear(renderer);
    AfficherPartie(partie,renderer);

    affichagescrollable.Draw(renderer);

    SDL_RenderPresent(renderer);
    SDL_Delay(16);
  end;
  if timerID <> 0 then
    SDL_RemoveTimer(timerID);
  DestroyTextures;
  TTF_CloseFont(font);
  affichagescrollable.detruire;
  SDL_Quit;
end.