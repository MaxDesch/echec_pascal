Unit echec_utilitaire;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, affichage_class, 
  SDL2 in 'SDL2-Pascal/units/sdl2.pas',
  SDL2_ttf in 'SDL2-Pascal/units/sdl2_ttf.pas';

const
  BLANC = 1;
  NOIR = -1;
  PION = 1;
  TOUR = 2;
  FOU = 3;
  CAVALIER = 4;
  DAME = 5;
  ROI = 6;
  VIDE = 0;
  EN_PASSANT = 100;
	DRAW = 10;

Type 
  TPos = record
    x,y:Integer;
  end;
  TCoup = record
    xDepart, yDepart: Integer;
    xArrivee, yArrivee: Integer;
    pieceDeplacee: Integer;
    pieceCapturee: Integer;
    promotion: Integer; // Pour les promotions de pions
    roque: Boolean; // Pour le roque
  end;

  TCase = record
    piece : Integer;
    coups_autoriser : array of TCoup;
  end;

  tab_Coup = array of TCoup; 

  Techiquier = record
    echiquier : array[0..7, 0..7] of TCase;
    roiblanc_x, roiblanc_y : Integer;
    roinoir_x, roinoir_y : Integer;
    grand_roque_blanc_possible,petit_roque_blanc_possible : Integer;
    grand_roque_noir_possible,petit_roque_noir_possible : Integer;
    tour_noir_gauche,tour_noir_droite : TPos;
    tour_blanc_gauche,tour_blanc_droite : TPos;
    en_passant_blanc,en_passant_noir :TPos
  end;

  TPartie_echec = record
    echiquier : Techiquier ;
    piece_selectione : TPos ;
    couleur_affichage : Integer ;
    couleur_joueur : Integer ;
    timer_blanc : Integer ;
    timer_noir : Integer ;
    gagnant : Integer ;
    timer_on, cliquable : Boolean; 
    affichage_coup_blanc,affichage_coup_noir : TAffichageScrollable;
    gestionaire : TGestionnaireTAffichageScrollable;
    font : PTTF_Font;
  end;

function InitialiserEchiquier(): Techiquier;
function EstPositionValide(x, y: Integer): Boolean;
function CoupsValides(echiquier: Techiquier; x, y: Integer; check_coup_valable: Boolean): tab_Coup;
function is_king_in_check(echiquier: Techiquier; color, kingX, kingY: Integer): Boolean;
procedure calculer_coup_couleur(var partie:TPartie_echec;couleur:Integer);
procedure gerer_clique(var partie:TPartie_echec;y,x:Integer;renderer : PSDL_Renderer);
function Initialisation_partie(): TPartie_echec;
procedure jouer_coup_definitivement(coup:TCoup;var partie:TPartie_echec; renderer : PSDL_Renderer);
procedure finir_partie(var partie:TPartie_echec;gagnant:Integer);
procedure diminuer_timer(var partie:TPartie_echec);

implementation

function InitialiserEchiquier(): Techiquier;
var
  i, j: Integer;
begin
  // Initialiser toutes les cases à VIDE
  for i := 0 to 7 do
    for j := 0 to 7 do
      Result.echiquier[i, j].piece := VIDE;
  // Placer les pions blancs
  for j := 0 to 7 do
    Result.echiquier[1, j].piece := PION;
  // Placer les pions noirs
  for j := 0 to 7 do
    Result.echiquier[6, j].piece := -PION;
  // Placer les autres pièces blanches
  Result.echiquier[0, 0].piece := TOUR;
  Result.echiquier[0, 1].piece := CAVALIER;
  Result.echiquier[0, 2].piece := FOU;
  Result.echiquier[0, 3].piece := DAME;
  Result.echiquier[0, 4].piece := ROI;
  Result.echiquier[0, 5].piece := FOU;
  Result.echiquier[0, 6].piece := CAVALIER;
  Result.echiquier[0, 7].piece := TOUR;
  // Placer les autres pièces noires
  Result.echiquier[7, 0].piece := -TOUR;
  Result.echiquier[7, 1].piece := -CAVALIER;
  Result.echiquier[7, 2].piece := -FOU;
  Result.echiquier[7, 3].piece := -DAME;
  Result.echiquier[7, 4].piece := -ROI;
  Result.echiquier[7, 5].piece := -FOU;
  Result.echiquier[7, 6].piece := -CAVALIER;
  Result.echiquier[7, 7].piece := -TOUR;
    // Placer les autres pièces blanches
  Result.echiquier[0, 0].coups_autoriser := [];
  Result.echiquier[0, 1].coups_autoriser := [];
  Result.echiquier[0, 2].coups_autoriser := [];
  Result.echiquier[0, 3].coups_autoriser := [];
  Result.echiquier[0, 4].coups_autoriser := [];
  Result.echiquier[0, 5].coups_autoriser := [];
  Result.echiquier[0, 6].coups_autoriser := [];
  Result.echiquier[0, 7].coups_autoriser := [];
  // Placer les autres pièces noires
  Result.echiquier[7, 0].coups_autoriser := [];
  Result.echiquier[7, 1].coups_autoriser := [];
  Result.echiquier[7, 2].coups_autoriser := [];
  Result.echiquier[7, 3].coups_autoriser := [];
  Result.echiquier[7, 4].coups_autoriser := [];
  Result.echiquier[7, 5].coups_autoriser := [];
  Result.echiquier[7, 6].coups_autoriser := [];
  Result.echiquier[7, 7].coups_autoriser := [];
  // Initialisation des autres variables
  Result.roiblanc_x := 0;
  Result.roiblanc_y := 4;
  Result.roinoir_x := 7;
  Result.roinoir_y := 4;
  Result.grand_roque_blanc_possible := 0;
  Result.grand_roque_noir_possible := 0;
  Result.petit_roque_blanc_possible := 0;
  Result.petit_roque_noir_possible := 0;
  Result.tour_noir_gauche.x := 7; Result.tour_noir_gauche.y := 7;
  Result.tour_noir_droite.x := 7; Result.tour_noir_droite.y := 0; 
  Result.tour_blanc_gauche.x := 0; Result.tour_blanc_gauche.y := 7;
  Result.tour_blanc_droite.x := 0; Result.tour_blanc_droite.y := 0;
  Result.en_passant_blanc.x := -1; Result.en_passant_blanc.y := 0;
  Result.en_passant_noir.x := -1; Result.en_passant_noir.y := 0;
end;

function Initialisation_partie(): TPartie_echec;
begin
  Result.echiquier := InitialiserEchiquier;
  Result.piece_selectione.x := -1 ;
  Result.piece_selectione.y := 0 ;
  Result.couleur_affichage := BLANC ;
  Result.couleur_joueur := BLANC ;
	Result.timer_blanc := 30*600 ;
	Result.timer_noir := 30*600 ;
	Result.gagnant := VIDE ;
	Result.timer_on := True; Result.cliquable := True;
	calculer_coup_couleur(Result,Result.couleur_joueur);
  Result.affichage_coup_blanc := TAffichageScrollable.Create(375, 60, 100, 200, 5, RGB(0,0,0));
  Result.affichage_coup_noir := TAffichageScrollable.Create(520, 60, 100, 200, 5, RGB(0,0,0));
  Result.gestionaire := TGestionnaireTAffichageScrollable.Create;
  Result.gestionaire.Ajout_Affichage(@Result.affichage_coup_blanc);
  Result.gestionaire.Ajout_Affichage(@Result.affichage_coup_noir);
  Result.font := TTF_OpenFont('font/gau_font_cube/GAU_cube_B.TTF', 13);
end;

function EstPositionValide(x, y: Integer): Boolean;
begin
  Result := (x >= 0) and (x < 8) and (y >= 0) and (y < 8);
end;

function Couleur_piece(piece:Integer):Integer;
begin
  if piece > 0 then
    Result := BLANC
  else if piece = VIDE then
		Result := VIDE
	else
		Result := NOIR;
end;

procedure jouer_coup(coup:TCoup;var echiquier:Techiquier);
begin
  echiquier.echiquier[coup.xArrivee][coup.yArrivee].piece := echiquier.echiquier[coup.xDepart][coup.yDepart].piece;
  echiquier.echiquier[coup.xDepart][coup.yDepart].piece := VIDE;
  if coup.pieceDeplacee = ROI then
  begin
    echiquier.roiblanc_x := coup.xArrivee;
    echiquier.roiblanc_y := coup.yArrivee;
    echiquier.grand_roque_blanc_possible += 1;
    echiquier.petit_roque_blanc_possible += 1;
  end;
  if coup.pieceDeplacee = -ROI then
  begin
    echiquier.roinoir_x := coup.xArrivee;
    echiquier.roinoir_y := coup.yArrivee;
    echiquier.grand_roque_noir_possible += 1;
    echiquier.petit_roque_noir_possible += 1;
  end;
  if coup.pieceDeplacee = TOUR then
  begin
    if (coup.xDepart = echiquier.tour_blanc_droite.x) and (coup.yDepart = echiquier.tour_blanc_droite.y) then
    begin
      echiquier.tour_blanc_droite.x := coup.xArrivee; echiquier.tour_blanc_droite.y := coup.yArrivee;
      echiquier.grand_roque_blanc_possible += 1
    end;
    if (coup.xDepart = echiquier.tour_blanc_gauche.x) and (coup.yDepart = echiquier.tour_blanc_gauche.y) then
    begin
      echiquier.tour_blanc_gauche.x := coup.xArrivee; echiquier.tour_blanc_gauche.y := coup.yArrivee;
      echiquier.petit_roque_blanc_possible += 1
    end;
  end;
  if coup.pieceDeplacee = -TOUR then
  begin
    if (coup.xDepart = echiquier.tour_noir_droite.x) and (coup.yDepart = echiquier.tour_noir_droite.y) then
    begin
      echiquier.tour_noir_droite.x := coup.xArrivee; echiquier.tour_noir_droite.y := coup.yArrivee;
      echiquier.grand_roque_noir_possible += 1
    end;
    if (coup.xDepart = echiquier.tour_noir_gauche.x) and (coup.yDepart = echiquier.tour_noir_gauche.y) then
    begin
      echiquier.tour_noir_gauche.x := coup.xArrivee; echiquier.tour_noir_gauche.y := coup.yArrivee;
      echiquier.petit_roque_noir_possible += 1
    end;
  end;
  if coup.roque then 
  begin
    if (coup.xArrivee = 7) and (coup.yArrivee = 6) then
    begin
      echiquier.echiquier[7,5].piece := echiquier.echiquier[7,7].piece;
      echiquier.echiquier[7,7].piece := VIDE;
    end;
    if (coup.xArrivee = 7) and (coup.yArrivee = 2) then
    begin
      echiquier.echiquier[7,3].piece := echiquier.echiquier[7,0].piece;
      echiquier.echiquier[7,0].piece := VIDE;
    end;
    if (coup.xArrivee = 0) and (coup.yArrivee = 6) then
    begin
      echiquier.echiquier[0,5].piece := echiquier.echiquier[0,7].piece;
      echiquier.echiquier[0,7].piece := VIDE;
    end;
    if (coup.xArrivee = 0) and (coup.yArrivee = 2) then
    begin
      echiquier.echiquier[0,3].piece := echiquier.echiquier[0,0].piece;
      echiquier.echiquier[0,0].piece := VIDE;
    end;
  end;
  if (coup.pieceDeplacee = PION) and (abs(coup.xArrivee - coup.xDepart) = 2) then
  begin
    echiquier.en_passant_blanc.x := coup.xArrivee;
    echiquier.en_passant_blanc.y := coup.yArrivee;
  end
  else
    echiquier.en_passant_blanc.x := -1;
  if (coup.pieceDeplacee = -PION) and (abs(coup.xArrivee - coup.xDepart) = 2) then
  begin
    echiquier.en_passant_noir.x := coup.xArrivee;
    echiquier.en_passant_noir.y := coup.yArrivee;
  end
  else
    echiquier.en_passant_noir.x := -1 ;
  if coup.pieceCapturee = EN_PASSANT then
    echiquier.echiquier[coup.xDepart][coup.yArrivee].piece := VIDE;
  
end;

procedure dejouer_coup(coup:TCoup;var echiquier:Techiquier);
begin
  echiquier.echiquier[coup.xDepart][coup.yDepart].piece := echiquier.echiquier[coup.xArrivee][coup.yArrivee].piece;
  echiquier.echiquier[coup.xArrivee][coup.yArrivee].piece := coup.pieceCapturee;
  if coup.pieceDeplacee = ROI then
  begin
    echiquier.roiblanc_x := coup.xDepart;
    echiquier.roiblanc_y := coup.yDepart;
    echiquier.grand_roque_blanc_possible -= 1;
    echiquier.petit_roque_blanc_possible -= 1;
  end;
  if coup.pieceDeplacee = -ROI then
  begin
    echiquier.roinoir_x := coup.xDepart;
    echiquier.roinoir_y := coup.yDepart;
    echiquier.grand_roque_noir_possible -= 1;
    echiquier.petit_roque_noir_possible -= 1;
  end;
  if coup.pieceDeplacee = TOUR then
  begin
    if (coup.xArrivee = echiquier.tour_blanc_droite.x) and (coup.yArrivee = echiquier.tour_blanc_droite.y) then
    begin
      echiquier.tour_blanc_droite.x := coup.xDepart; echiquier.tour_blanc_droite.y := coup.yDepart;
      echiquier.grand_roque_blanc_possible -= 1;
    end;
    if (coup.xArrivee = echiquier.tour_blanc_gauche.x) and (coup.yArrivee = echiquier.tour_blanc_gauche.y) then
    begin
      echiquier.tour_blanc_gauche.x := coup.xDepart; echiquier.tour_blanc_gauche.y := coup.yDepart;
      echiquier.petit_roque_blanc_possible -= 1;
    end;
  end;
  if coup.pieceDeplacee = -TOUR then
  begin
    if (coup.xArrivee = echiquier.tour_noir_droite.x) and (coup.yArrivee = echiquier.tour_noir_droite.y) then
    begin
      echiquier.tour_noir_droite.x := coup.xDepart; echiquier.tour_noir_droite.y := coup.yDepart;
      echiquier.grand_roque_noir_possible -= 1;
    end;
    if (coup.xArrivee = echiquier.tour_noir_gauche.x) and (coup.yArrivee = echiquier.tour_noir_gauche.y) then
    begin
      echiquier.tour_noir_gauche.x := coup.xDepart; echiquier.tour_noir_gauche.y := coup.yDepart;
      echiquier.petit_roque_noir_possible -= 1;
    end;
  end;
  if coup.roque then 
  begin
    if (coup.xArrivee = 7) and (coup.yArrivee = 6) then
    begin
      echiquier.echiquier[7,7].piece := echiquier.echiquier[7,5].piece;
      echiquier.echiquier[7,5].piece := VIDE;
    end;
    if (coup.xArrivee = 7) and (coup.yArrivee = 2) then
    begin
      echiquier.echiquier[7,0].piece := echiquier.echiquier[7,3].piece;
      echiquier.echiquier[7,3].piece := VIDE;
    end;
    if (coup.xArrivee = 0) and (coup.yArrivee = 6) then
    begin
      echiquier.echiquier[0,7].piece := echiquier.echiquier[0,5].piece;
      echiquier.echiquier[0,5].piece := VIDE;
    end;
    if (coup.xArrivee = 0) and (coup.yArrivee = 2) then
    begin
      echiquier.echiquier[0,0].piece := echiquier.echiquier[0,3].piece;
      echiquier.echiquier[0,3].piece := VIDE;
    end;
  end;
  if (coup.pieceDeplacee = PION) and (abs(coup.xArrivee - coup.xDepart) = 2) then
    echiquier.en_passant_blanc.x := -1;
  
  if (coup.pieceDeplacee = -PION) and (abs(coup.xArrivee - coup.xDepart) = 2) then
    echiquier.en_passant_noir.x := -1;
  
  if coup.pieceCapturee = EN_PASSANT then
  begin
    echiquier.echiquier[coup.xDepart][coup.yArrivee].piece := -coup.pieceDeplacee;
    echiquier.echiquier[coup.xArrivee][coup.yArrivee].piece := VIDE;
  end;
end;

procedure diminuer_timer(var partie:TPartie_echec);
begin
	if not partie.timer_on then
		Exit;
	if partie.couleur_joueur = BLANC then
	begin
		partie.timer_blanc -= 1;
		if partie.timer_blanc = 0 then
			finir_partie(partie,NOIR);
	end
	else
	begin
		partie.timer_noir -= 1;
		if partie.timer_noir = 0 then
				finir_partie(partie,BLANC);
	end;
end;

procedure gerer_clique(var partie:TPartie_echec;y,x:Integer; renderer : PSDL_Renderer);
var piece, i : Integer; tab_coup : array of TCoup; 
begin
	if not partie.cliquable then 
		Exit;
  if EstPositionValide(x,y) then
  if partie.couleur_affichage = BLANC then
    begin
      x := 7 - x;
      y := 7 - y;
    end;
  begin
    piece := partie.echiquier.echiquier[x][y].piece;
    if (x = partie.piece_selectione.x) and (y = partie.piece_selectione.y) then
    begin
      partie.piece_selectione.x := -1;
      Exit;
    end;
    if Couleur_piece(piece) = partie.couleur_joueur then
    begin
      partie.piece_selectione.x := x;
      partie.piece_selectione.y := y;
      Exit;
    end;
    if partie.piece_selectione.x > -1 then
    begin
      tab_coup := partie.echiquier.echiquier[partie.piece_selectione.x][partie.piece_selectione.y].coups_autoriser;
      for i := 0 to Length(tab_coup) - 1 do
      begin
        if (x = tab_coup[i].xArrivee) and (y = tab_coup[i].yArrivee) then
          jouer_coup_definitivement(tab_coup[i],partie, renderer)
      end;
    end;
  end;
  partie.piece_selectione.x := -1;
end;


procedure ajouter_coup(var coups: tab_Coup; xD, yD, xA, yA, pieceDep, pieceCap, promo: Integer; rock:Boolean;echiquier:Techiquier; check_coup_valable: Boolean);
var
  newCoup: TCoup; bool : Boolean;
begin
  newCoup.xDepart := xD;
  newCoup.yDepart := yD;
  newCoup.xArrivee := xA;
  newCoup.yArrivee := yA;
  newCoup.pieceDeplacee := pieceDep;
  newCoup.pieceCapturee := pieceCap;
  newCoup.promotion := promo;
  newCoup.roque := rock;
  if check_coup_valable then
  begin
    jouer_coup(newCoup,echiquier);
    if Couleur_piece(pieceDep) = BLANC then
     bool := is_king_in_check(echiquier,BLANC,echiquier.roiblanc_x,echiquier.roiblanc_y)
    else 
      bool := is_king_in_check(echiquier,NOIR,echiquier.roinoir_x,echiquier.roinoir_y);
    dejouer_coup(newCoup,echiquier);
    if bool then Exit;
  end;
  SetLength(coups, Length(coups) + 1);
  coups[High(coups)] := newCoup;
end;

procedure check_direction(echiquier: Techiquier; x,y, dir_x, dir_y: Integer; var tab: tab_Coup; check_coup_valable: Boolean);
var
  start_x, start_y, piece: Integer;
begin
  start_x := x;
  start_y := y;
  x := x + dir_x;
  y := y + dir_y;
  piece := echiquier.echiquier[start_x, start_y].piece;
  while EstPositionValide(x, y) and (echiquier.echiquier[x,y].piece = VIDE) do
  begin
    ajouter_coup(tab, start_x, start_y, x, y, piece, echiquier.echiquier[x, y].piece, 0, False, echiquier, check_coup_valable);
    x := x + dir_x;
    y := y + dir_y;
  end;
  if EstPositionValide(x, y) and (Couleur_piece(echiquier.echiquier[x,y].piece) <> Couleur_piece(piece)) then
    ajouter_coup(tab, start_x, start_y, x, y, piece, echiquier.echiquier[x, y].piece, 0, False, echiquier, check_coup_valable);
end;

function is_king_in_check(echiquier: Techiquier; color, kingX, kingY: Integer): Boolean;
var 
  i, j: Integer;
  enemyMoves: tab_Coup;
  move: TCoup;
begin
  // Cette fonction doit être implémentée pour vérifier si le roi de la couleur donnée est en échec.
  // Pour l'instant, elle retourne toujours False.
  Result := False;
  for i := 0 to 7 do
    for j := 0 to 7 do
    begin
      if (color = BLANC) and (echiquier.echiquier[i, j].piece < 0) then
      begin
        enemyMoves := CoupsValides(echiquier, i, j, False);
        for move in enemyMoves do
        begin
          if (move.xArrivee = kingX) and (move.yArrivee = kingY) then
          begin
            Result := True;
            Exit;
          end;
        end;
      end
      else if (color = NOIR) and (echiquier.echiquier[i, j].piece > 0) then
      begin
        enemyMoves := CoupsValides(echiquier, i, j, False);
        for move in enemyMoves do
        begin
          if (move.xArrivee = kingX) and (move.yArrivee = kingY) then
          begin
            Result := True;
            Exit;
          end;
        end;
      end;
    end;
end;

function abs(Val: Integer): Integer;
begin
  if Val < 0 then
    Result := -Val
  else
    Result := Val;
end;

function CoupsValides(echiquier: Techiquier; x, y: Integer; check_coup_valable: Boolean): tab_Coup; // la Tibs
var 
  i: Integer;
  moves: array of array of Integer;
  newX, newY: Integer;
begin
  Result := [];
  // Cette fonction doit être implémentée pour retourner les coups valides
  // pour la pièce située en (x, y) sur l'échiquier.
  // Pour l'instant, elle retourne un tableau vide.
  case abs(echiquier.echiquier[x, y].piece) of
    PION: begin
      // Logique des coups valides pour le pion
      if (echiquier.echiquier[x, y].piece = PION)then
      begin
        // Pion blanc
        if EstPositionValide(x + 1, y) and (echiquier.echiquier[x + 1, y].piece = VIDE) then
        begin
          ajouter_coup(Result, x, y, x + 1, y, PION, VIDE, 0, False, echiquier, check_coup_valable);
        end;
        // Ajouter la logique pour les captures et les promotions
        if EstPositionValide(x + 2, y) and (x = 1) and (echiquier.echiquier[x + 1, y].piece = VIDE) and (echiquier.echiquier[x + 2, y].piece = VIDE) then
        begin
          ajouter_coup(Result, x, y, x + 2, y, PION, VIDE, 0, False, echiquier, check_coup_valable);
        end;
        if EstPositionValide(x + 1, y + 1) and (echiquier.echiquier[x + 1, y + 1].piece < 0) then
        begin
          ajouter_coup(Result, x, y, x + 1, y + 1, PION, echiquier.echiquier[x + 1, y + 1].piece, 0, False, echiquier, check_coup_valable);
        end;
        if EstPositionValide(x + 1, y - 1) and (echiquier.echiquier[x + 1, y - 1].piece < 0) then
        begin
          ajouter_coup(Result, x, y, x + 1, y - 1, PION, echiquier.echiquier[x + 1, y - 1].piece, 0, False, echiquier, check_coup_valable);
        end;
        // en passant
        if (x = echiquier.en_passant_noir.x) and  (y + 1 = echiquier.en_passant_noir.y) then
        begin
          ajouter_coup(Result, x, y, x + 1, y + 1, PION, EN_PASSANT, 0, False, echiquier, check_coup_valable);
        end;
        if (x = echiquier.en_passant_noir.x) and  (y - 1 = echiquier.en_passant_noir.y) then
        begin
          ajouter_coup(Result, x, y, x + 1, y - 1, PION, EN_PASSANT, 0, False, echiquier, check_coup_valable);
        end;
      end
      else
      begin
        // Pion noir
        if EstPositionValide(x - 1, y) and (echiquier.echiquier[x - 1, y].piece = VIDE) then
        begin
          ajouter_coup(Result, x, y, x - 1, y, -PION, VIDE, 0, False, echiquier, check_coup_valable);
        end;
        // Ajouter la logique pour les captures et les promotions
        if EstPositionValide(x - 2, y) and (x = 6) and (echiquier.echiquier[x - 1, y].piece = VIDE) and (echiquier.echiquier[x - 2, y].piece = VIDE) then
        begin
          ajouter_coup(Result, x, y, x - 2, y, -PION, VIDE, 0, False, echiquier, check_coup_valable);
        end;
        if EstPositionValide(x - 1, y - 1) and (echiquier.echiquier[x - 1, y - 1].piece > 0) then
        begin
          ajouter_coup(Result, x, y, x - 1, y - 1, -PION, echiquier.echiquier[x - 1, y - 1].piece, 0, False, echiquier, check_coup_valable);
        end;
        if EstPositionValide(x - 1, y + 1) and (echiquier.echiquier[x - 1, y + 1].piece > 0) then
        begin
          ajouter_coup(Result, x, y, x - 1, y + 1, -PION, echiquier.echiquier[x - 1, y + 1].piece, 0, False, echiquier, check_coup_valable);
        end;
        if (x = echiquier.en_passant_noir.x) and  (y + 1 = echiquier.en_passant_noir.y) then
        begin
          ajouter_coup(Result, x, y, x - 1, y + 1, -PION, EN_PASSANT, 0, False, echiquier, check_coup_valable);
        end;
        if (x = echiquier.en_passant_noir.x) and  (y - 1 = echiquier.en_passant_noir.y) then
        begin
          ajouter_coup(Result, x, y, x - 1, y - 1, -PION, EN_PASSANT, 0, False, echiquier, check_coup_valable);
        end;
      end
    end;
    TOUR: 
    begin
      // Logique des coups valides pour la tour
      check_direction(echiquier, x, y, 1, 0, Result, check_coup_valable); // Vers le bas
      check_direction(echiquier, x, y, -1, 0, Result, check_coup_valable); // Vers le haut
      check_direction(echiquier, x, y, 0, 1, Result, check_coup_valable); // Vers la droite
      check_direction(echiquier, x, y, 0, -1, Result, check_coup_valable); // Vers la gauche
    end;
    FOU : begin
      // Logique des coups valides pour le fou
      check_direction(echiquier, x, y, 1, 1, Result, check_coup_valable); // Diagonale bas-droite
      check_direction(echiquier, x, y, 1, -1, Result, check_coup_valable); // Diagonale bas-gauche
      check_direction(echiquier, x, y, -1, 1, Result, check_coup_valable); // Diagonale haut-droite
      check_direction(echiquier, x, y, -1, -1, Result, check_coup_valable); // Diagonale haut-gauche
    end;
    CAVALIER: begin
      // Logique des coups valides pour le cavalier
      moves := [[2, 1], [2, -1], [-2, 1], [-2, -1], [1, 2], [1, -2], [-1, 2], [-1, -2]];
      for i := 0 to 7 do
      begin
        newX := x + moves[i, 0];
        newY := y + moves[i, 1];
        if EstPositionValide(newX, newY) then
        begin
          if (echiquier.echiquier[newX, newY].piece = VIDE) or ((echiquier.echiquier[x, y].piece > 0) and (echiquier.echiquier[newX, newY].piece < 0)) or ((echiquier.echiquier[x, y].piece < 0) and (echiquier.echiquier[newX, newY].piece > 0)) then
          begin
            ajouter_coup(Result, x, y, newX, newY, echiquier.echiquier[x, y].piece, echiquier.echiquier[newX, newY].piece, 0, False, echiquier, check_coup_valable);
          end;
        end;
      end;
    end;
    DAME: begin
      // Logique des coups valides pour la dame
      check_direction(echiquier, x, y, 1, 0, Result, check_coup_valable); // Vers le bas
      check_direction(echiquier, x, y, -1, 0, Result, check_coup_valable); // Vers le haut
      check_direction(echiquier, x, y, 0, 1, Result, check_coup_valable); // Vers la droite
      check_direction(echiquier, x, y, 0, -1, Result, check_coup_valable); // Vers la gauche
      check_direction(echiquier, x, y, 1, 1, Result, check_coup_valable); // Diagonale bas-droite
      check_direction(echiquier, x, y, 1, -1, Result, check_coup_valable); // Diagonale bas-gauche
      check_direction(echiquier, x, y, -1, 1, Result, check_coup_valable); // Diagonale haut-droite
      check_direction(echiquier, x, y,-1,-1 ,Result, check_coup_valable); // Diagonale haut-gauche
    end;
    ROI: 
    begin
      // Logique des coups valides pour le roi
      moves := [[1, 0], [1, 1], [0, 1], [-1, 1], [-1, 0], [-1, -1], [0, -1], [1, -1]];
      for i := 0 to 7 do
      begin
        newX := x + moves[i, 0];
        newY := y + moves[i, 1];
        if EstPositionValide(newX, newY) and ((echiquier.echiquier[newX, newY].piece = VIDE) or (Couleur_piece(echiquier.echiquier[x, y].piece) <> Couleur_piece(echiquier.echiquier[newX, newY].piece))) then
              ajouter_coup(Result, x, y, newX, newY, echiquier.echiquier[x, y].piece, echiquier.echiquier[newX, newY].piece, 0, False, echiquier, check_coup_valable);
      end;
      if echiquier.echiquier[x, y].piece = ROI then
      begin
        if (echiquier.petit_roque_blanc_possible = 0) and (echiquier.echiquier[0, 5].piece = VIDE) and (echiquier.echiquier[0, 6].piece = VIDE) then 
          ajouter_coup(Result, x, y, 0, 6, echiquier.echiquier[x, y].piece, VIDE, 0, True, echiquier, check_coup_valable);
        if (echiquier.grand_roque_blanc_possible = 0) and (echiquier.echiquier[0, 3].piece = VIDE) and (echiquier.echiquier[0, 2].piece = VIDE) and (echiquier.echiquier[0, 1].piece = VIDE) then
          ajouter_coup(Result, x, y, 0, 2, echiquier.echiquier[x, y].piece, VIDE, 0, True, echiquier, check_coup_valable);
      end
      else
      begin
        if (echiquier.petit_roque_noir_possible = 0) and (echiquier.echiquier[7, 5].piece = VIDE) and (echiquier.echiquier[7, 6].piece = VIDE) then 
          ajouter_coup(Result, x, y, 7, 6, echiquier.echiquier[x, y].piece, VIDE, 0, True, echiquier, check_coup_valable);
        if (echiquier.grand_roque_noir_possible = 0) and (echiquier.echiquier[7, 3].piece = VIDE) and (echiquier.echiquier[7, 2].piece = VIDE) and (echiquier.echiquier[7, 1].piece = VIDE) then
          ajouter_coup(Result, x, y, 7, 2, echiquier.echiquier[x, y].piece, VIDE, 0, True, echiquier, check_coup_valable);
      end;
    end;
  end;
end;

procedure calculer_coup_couleur(var partie:TPartie_echec;couleur:Integer);
var i,j:Integer;
begin
  for i := 0 to 7 do
    for j := 0 to 7 do
    begin
    	if Couleur_piece(partie.echiquier.echiquier[i][j].piece) = couleur then
      begin
        partie.echiquier.echiquier[i][j].coups_autoriser := CoupsValides(partie.echiquier, i, j, True);
      end;
    end;
end;

function piece_to_lettre(piece:Integer): String;
begin
  case abs(piece) of
    PION : Result := '';
    TOUR : Result := 'r';
    FOU : Result := 'b';
    CAVALIER : Result := 'c';
    DAME : Result := 'q';
    ROI : Result := 'k';
  end;
end;

function Coup_to_string(coup:TCoup):String;
begin
  Result := '';
  Result += piece_to_lettre(coup.pieceDeplacee) + Char(97 + 7 - coup.yDepart) + IntToStr(coup.xDepart + 1) + '->' + Char(97 + 7 - coup.yArrivee) + IntToStr(coup.xArrivee + 1);
  WriteLn('ahaha');
  if coup.pieceCapturee <> VIDE then 
    Result += ' x'
end;

procedure jouer_coup_definitivement(coup:TCoup;var partie:TPartie_echec;renderer : PSDL_Renderer);
begin
	jouer_coup(coup,partie.echiquier);
  WriteLn('ez doi');
  if partie.couleur_joueur = BLANC then
    partie.affichage_coup_blanc.Ajouter_Surface(TTF_RenderText_Solid(partie.font, PChar(Coup_to_string(coup)), RGB(255,255,255)),renderer)
  else
    partie.affichage_coup_noir.Ajouter_Surface(TTF_RenderText_Solid(partie.font, PChar(Coup_to_string(coup)), RGB(255,255,255)),renderer);
  WriteLn('ez doi');
	partie.couleur_joueur := -partie.couleur_joueur;
	calculer_coup_couleur(partie,partie.couleur_joueur);
end;

procedure finir_partie(var partie:TPartie_echec;gagnant:Integer);
begin
	partie.gagnant := gagnant ;
	partie.cliquable := False ;
	partie.timer_on := False ;
end;

end.