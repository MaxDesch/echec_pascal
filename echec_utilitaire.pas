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
  TCoup = record
    xDepart, yDepart: Integer;
    xArrivee, yArrivee: Integer;
    pieceDeplacee: Integer;
    pieceCapturee: Integer;
    promotion: Integer; // Pour les promotions de pions
    rock: Boolean; // Pour le roque
  end;
  tab_Coup = array of TCoup; 
  Techiquier = record
  echiquier : array[0..7, 0..7] of Integer;
  roiblanc_x, roiblanc_y : Integer;
  roinoir_x, roinoir_y : Integer;
  roque_blanc_possible : Boolean;
  roque_noir_possible : Boolean;
  end;

function InitialiserEchiquier(): Techiquier;
function EstPositionValide(x, y: Integer): Boolean;
function CoupsValides(echiquier: Techiquier; x, y: Integer; calcule_echec: Boolean): tab_Coup;

implementation

function InitialiserEchiquier(): Techiquier;
var
  i, j: Integer;
begin
  // Initialiser toutes les cases à VIDE
  for i := 0 to 7 do
    for j := 0 to 7 do
      Result.echiquier[i, j] := VIDE;
  // Placer les pions blancs
  for j := 0 to 7 do
    Result.echiquier[1, j] := PION;
  // Placer les pions noirs
  for j := 0 to 7 do
    Result.echiquier[6, j] := -PION;
  // Placer les autres pièces blanches
  Result.echiquier[0, 0] := TOUR;
  Result.echiquier[0, 1] := CAVALIER;
  Result.echiquier[0, 2] := FOU;
  Result.echiquier[0, 3] := DAME;
  Result.echiquier[0, 4] := ROI;
  Result.echiquier[0, 5] := FOU;
  Result.echiquier[0, 6] := CAVALIER;
  Result.echiquier[0, 7] := TOUR;
  // Placer les autres pièces noires
  Result.echiquier[7, 0] := -TOUR;
  Result.echiquier[7, 1] := -CAVALIER;
  Result.echiquier[7, 2] := -FOU;
  Result.echiquier[7, 3] := -DAME;
  Result.echiquier[7, 4] := -ROI;
  Result.echiquier[7, 5] := -FOU;
  Result.echiquier[7, 6] := -CAVALIER;
  Result.echiquier[7, 7] := -TOUR;
end;

function EstPositionValide(x, y: Integer): Boolean;
begin
  Result := (x >= 0) and (x < 8) and (y >= 0) and (y < 8);
end;

procedure ajouter_coup(var coups: tab_Coup; xD, yD, xA, yA, pieceDep, pieceCap, promo: Integer; rock: Boolean);
var
  newCoup: TCoup;
begin
  newCoup.xDepart := xD;
  newCoup.yDepart := yD;
  newCoup.xArrivee := xA;
  newCoup.yArrivee := yA;
  newCoup.pieceDeplacee := pieceDep;
  newCoup.pieceCapturee := pieceCap;
  newCoup.promotion := promo;
  newCoup.rock := rock;
  SetLength(coups, Length(coups) + 1);
  coups[High(coups)] := newCoup;
end;

procedure check_direction(echiquier: Techiquier; x,y, dir_x, dir_y: Integer; var Result: tab_Coup);
var
  start_x, start_y, piece: Integer;
begin
  Result := [];
  start_x := x;
  start_y := y;
  piece := echiquier.echiquier[start_x, start_y];
  while EstPositionValide(x, y) and (echiquier.echiquier[x, y] = VIDE) do
  begin
    x := x + dir_x;
    y := y + dir_y;
    ajouter_coup(Result, start_x, start_y, x, y, piece, echiquier.echiquier[x, y], 0, False);
  end;
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
      if (color = BLANC) and (echiquier.echiquier[i, j] < 0) then
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
      else if (color = NOIR) and (echiquier.echiquier[i, j] > 0) then
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
function CoupsValides(echiquier: Techiquier; x, y: Integer; calcule_echec: Boolean): tab_Coup;
var 
  i: Integer;
  moves: array of array of Integer;
  newX, newY: Integer;
  color: Integer;
begin
  Result := [];
  // Cette fonction doit être implémentée pour retourner les coups valides
  // pour la pièce située en (x, y) sur l'échiquier.
  // Pour l'instant, elle retourne un tableau vide.
  case abs(echiquier.echiquier[x, y]) of
    PION: begin
      // Logique des coups valides pour le pion
      if (echiquier.echiquier[x, y] = PION)then
      begin
        // Pion blanc
        if EstPositionValide(x + 1, y) and (echiquier.echiquier[x + 1, y] = VIDE) then
        begin
          ajouter_coup(Result, x, y, x + 1, y, PION, VIDE, 0, False);
        end;
        // Ajouter la logique pour les captures et les promotions
        if EstPositionValide(x + 2, y) and (x = 1) and (echiquier.echiquier[x + 1, y] = VIDE) and (echiquier.echiquier[x + 2, y] = VIDE) then
        begin
          ajouter_coup(Result, x, y, x + 2, y, PION, VIDE, 0, False);
        end;
        if EstPositionValide(x + 1, y + 1) and (echiquier.echiquier[x + 1, y + 1] <> VIDE) and (echiquier.echiquier[x + 1, y + 1] < 0) then
        begin
          ajouter_coup(Result, x, y, x + 1, y + 1, PION, echiquier.echiquier[x + 1, y + 1], 0, False);
        end;
      end
      else
      begin
        // Pion noir
        if EstPositionValide(x - 1, y) and (echiquier.echiquier[x - 1, y] = VIDE) then
        begin
          ajouter_coup(Result, x, y, x - 1, y, -PION, VIDE, 0, False);
        end;
        // Ajouter la logique pour les captures et les promotions
        if EstPositionValide(x - 2, y) and (x = 6) and (echiquier.echiquier[x - 1, y] = VIDE) and (echiquier.echiquier[x - 2, y] = VIDE) then
        begin
          ajouter_coup(Result, x, y, x - 2, y, -PION, VIDE, 0, False);
        end;
        if EstPositionValide(x - 1, y - 1) and (echiquier.echiquier[x - 1, y - 1] <> VIDE) and (echiquier.echiquier[x - 1, y - 1] > 0) then
        begin
          ajouter_coup(Result, x, y, x - 1, y - 1, -PION, echiquier.echiquier[x - 1, y - 1], 0, False);
        end;
      end
    end;
    TOUR: 
    begin
      // Logique des coups valides pour la tour
      check_direction(echiquier, x, y, 1, 0, Result); // Vers le bas
      check_direction(echiquier, x, y, -1, 0, Result); // Vers le haut
      check_direction(echiquier, x, y, 0, 1, Result); // Vers la droite
      check_direction(echiquier, x, y, 0, -1, Result); // Vers la gauche
    end;
    (FOU or -FOU): begin
      // Logique des coups valides pour le fou
      check_direction(echiquier, x, y, 1, 1, Result); // Diagonale bas-droite
      check_direction(echiquier, x, y, 1, -1, Result); // Diagonale bas-gauche
      check_direction(echiquier, x, y, -1, 1, Result); // Diagonale haut-droite
      check_direction(echiquier, x, y, -1, -1, Result); // Diagonale haut-gauche
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
          if (echiquier.echiquier[newX, newY] = VIDE) or ((echiquier.echiquier[x, y] > 0) and (echiquier.echiquier[newX, newY] < 0)) or ((echiquier.echiquier[x, y] < 0) and (echiquier.echiquier[newX, newY] > 0)) then
          begin
            ajouter_coup(Result, x, y, newX, newY, echiquier.echiquier[x, y], echiquier.echiquier[newX, newY], 0, False);
          end;
        end;
      end;
    end;
    DAME: begin
      // Logique des coups valides pour la dame
      check_direction(echiquier, x, y, 1, 0, Result); // Vers le bas
      check_direction(echiquier, x, y, -1, 0, Result); // Vers le haut
      check_direction(echiquier, x, y, 0, 1, Result); // Vers la droite
      check_direction(echiquier, x, y, 0, -1, Result); // Vers la gauche
      check_direction(echiquier, x, y, 1, 1, Result); // Diagonale bas-droite
      check_direction(echiquier, x, y, 1, -1, Result); // Diagonale bas-gauche
      check_direction(echiquier, x, y, -1, 1, Result); // Diagonale haut-droite
      check_direction(echiquier, x, y,-1,-1 ,Result); // Diagonale haut-gauche
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
        begin
          if echiquier.echiquier[newX, newY] >0 then
            color := blanc
          else color := noir;
          if ((echiquier.echiquier[newX, newY] = VIDE) or ((echiquier.echiquier[x, y] > 0) and (echiquier.echiquier[newX, newY] < 0)) or ((echiquier.echiquier[x, y] < 0) and (echiquier.echiquier[newX, newY] > 0))) and is_king_in_check(echiquier, color, newX, newY) then
          begin
            ajouter_coup(Result, x, y, newX, newY, echiquier.echiquier[x, y], echiquier.echiquier[newX, newY], 0, False);
          end;
        end;
      end;
    end;
  end;

end;

end.