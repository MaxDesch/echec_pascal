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
  timerID :TSDL_TimerID;

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
  initialiserMenuPrincipal;
  
  timerID := SDL_AddTimer(100, @TimerCallback, nil);

  ratioscreen_x :=  SCREEN_WIDTH / actual_screen_width;
  ratioscreen_y :=  SCREEN_HEIGHT / actual_screen_heigth;

  partie := Initialisation_partie();
  partie.gestionaire.Definir_Scalaire(@ratioscreen_x,@ratioscreen_y);
  running := True;

  while running do
  begin
  if SDL_RenderSetLogicalSize(renderer, SCREEN_WIDTH, SCREEN_HEIGHT) <> 0 then
    Halt;
  while SDL_PollEvent(@event) <> 0 do
  begin
    case MODE_AFFICHAGE of
      MAINMENU: gerer_event_menu(event, MenuPrincipale);
      PARTIE_ECHEC: gerer_event_partie(event, partie);
    end;
    
    case event.type_ of
      SDL_QUITEV: running := False;
      SDL_KEYDOWN: 
      // debug pas besoin de lire les événements clavier pour l'instant
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
            WriteLn(Ord('a'));
            //affichagescrollable.Ajouter_Surface(TTF_RenderText_Solid(font, 'ez', RGB(255,255,255)),renderer);
          end;
        end;
      end;
      
      end;
    end;
    SDL_SetRenderDrawColor(renderer, 125, 125, 125, 255);
    SDL_RenderClear(renderer);

    // Affichage selon le mode
    case MODE_AFFICHAGE of
      MAINMENU: 
        MenuPrincipale.Draw(renderer);
      PARTIE_ECHEC:
        AfficherPartie(partie,renderer);
    end;


    SDL_RenderPresent(renderer);
    SDL_Delay(16);
  end;


  
  if timerID <> 0 then
    SDL_RemoveTimer(timerID);
  DestroyTextures;
  TTF_CloseFont(font);
  TTF_CloseFont(font_detailler);
  partie.gestionaire.detruire;
  SDL_Quit;
end.