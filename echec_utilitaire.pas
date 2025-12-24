Unit echec_utilitaire;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

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

Type 
  TPos = record
  x,y:Integer
  end;
  TCoup = record
    xDepart, yDepart: Integer;
    xArrivee, yArrivee: Integer;
    pieceDeplacee: Integer;
    pieceCapturee: Integer;
    promotion: Integer; // Pour les promotions de pions
    rock: Boolean; // Pour le roque
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
  roque_blanc_possible : Boolean;
  roque_noir_possible : Boolean;
  end;

  TPartie_echec = record
  echiquier : Techiquier ;
  piece_selectione : TPos ;
  couleur_affichage : Integer ;
  couleur_joueur : Integer ;
  end;

function InitialiserEchiquier(): Techiquier;
function EstPositionValide(x, y: Integer): Boolean;
function CoupsValides(echiquier: Techiquier; x, y: Integer; calcule_echec, check_coup_valable: Boolean): tab_Coup;
function is_king_in_check(echiquier: Techiquier; color, kingX, kingY: Integer): Boolean;
procedure calculer_coup_couleur(var partie:TPartie_echec;couleur:Integer);
procedure gerer_clique(var partie:TPartie_echec;y,x:Integer);
function Initialisation_partie(): TPartie_echec;

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
  Result.roque_blanc_possible := True;
  Result.roque_noir_possible := True;
end;

function Initialisation_partie(): TPartie_echec;
begin
  Result.echiquier := InitialiserEchiquier;
  Result.piece_selectione.x := -1;
  Result.piece_selectione.y := 0;
  Result.couleur_affichage := BLANC ;
  Result.couleur_joueur := BLANC ;
end;

function EstPositionValide(x, y: Integer): Boolean;
begin
  Result := (x >= 0) and (x < 8) and (y >= 0) and (y < 8);
end;

function Couleur_piece(piece:Integer):Integer;
begin
  if piece > 0 then
    Result := blanc
  else Result := noir;
end;

procedure jouer_coup(coup:TCoup;var echiquier:Techiquier);
begin
  echiquier.echiquier[coup.xArrivee][coup.yArrivee].piece := echiquier.echiquier[coup.xDepart][coup.yDepart].piece;
  echiquier.echiquier[coup.xDepart][coup.yDepart].piece := VIDE;
end;

procedure dejouer_coup(coup:TCoup;var echiquier:Techiquier);
begin
  echiquier.echiquier[coup.xDepart][coup.yDepart].piece := echiquier.echiquier[coup.xArrivee][coup.yArrivee].piece;
  echiquier.echiquier[coup.xArrivee][coup.yArrivee].piece := coup.pieceCapturee;
end;

procedure gerer_clique(var partie:TPartie_echec;y,x:Integer);
var piece, i : Integer; tab_coup : array of TCoup; 
begin
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
          jouer_coup(tab_coup[i],partie.echiquier)
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
  newCoup.rock := rock;
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
        enemyMoves := CoupsValides(echiquier, i, j, False, False);
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
        enemyMoves := CoupsValides(echiquier, i, j, False, False);
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

function CoupsValides(echiquier: Techiquier; x, y: Integer; calcule_echec, check_coup_valable: Boolean): tab_Coup;
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
        if EstPositionValide(x + 1, y + 1) and (echiquier.echiquier[x + 1, y + 1].piece <> VIDE) and (echiquier.echiquier[x + 1, y + 1].piece < 0) then
        begin
          ajouter_coup(Result, x, y, x + 1, y + 1, PION, echiquier.echiquier[x + 1, y + 1].piece, 0, False, echiquier, check_coup_valable);
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
        if EstPositionValide(x - 1, y - 1) and (echiquier.echiquier[x - 1, y - 1].piece <> VIDE) and (echiquier.echiquier[x - 1, y - 1].piece > 0) then
        begin
          ajouter_coup(Result, x, y, x - 1, y - 1, -PION, echiquier.echiquier[x - 1, y - 1].piece, 0, False, echiquier, check_coup_valable);
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
    (FOU or -FOU): begin
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
      if calcule_echec then
        Exit;
      // Logique des coups valides pour le roi
      moves := [[1, 0], [1, 1], [0, 1], [-1, 1], [-1, 0], [-1, -1], [0, -1], [1, -1]];
      for i := 0 to 7 do
      begin
        newX := x + moves[i, 0];
        newY := y + moves[i, 1];
        if EstPositionValide(newX, newY) then
          if (echiquier.echiquier[newX, newY].piece = VIDE) or ((echiquier.echiquier[x, y].piece > 0) and (echiquier.echiquier[newX, newY].piece < 0)) or ((echiquier.echiquier[x, y].piece < 0) and (echiquier.echiquier[newX, newY].piece > 0)) then
            begin
              ajouter_coup(Result, x, y, newX, newY, echiquier.echiquier[x, y].piece, echiquier.echiquier[newX, newY].piece, 0, False, echiquier, check_coup_valable);
            end;
        
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
        partie.echiquier.echiquier[i][j].coups_autoriser := CoupsValides(partie.echiquier, i, j, True, True);
        Writeln('nb coup pos (',i,' ',j,'): ',Length(partie.echiquier.echiquier[i][j].coups_autoriser));
      end;
    end;
end;


end.