unit ScryfallObjects;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

implementation
{ Symbols
'object':
  begin
    // Ignore
  end;
'symbol':
  begin
    if not(Node.Kind = nkString) then
      begin
        MemoMessage('TypeError for symbol expected nkString got ' + JSONKindToString(Node));
      end;
  end;
'svg_uri':
  begin
    if not(Node.Kind = nkString) then
      begin
        MemoMessage('TypeError for svg_uri expected nkString got ' + JSONKindToString(Node));
      end;
  end;
'loose_variant':
  begin
    // Ignore
  end;
'english':
  begin
    if not(Node.Kind = nkString) then
      begin
        MemoMessage('TypeError for english expected nkString got ' + JSONKindToString(Node));
      end;
  end;
'transposable':
  begin
    if not(Node.Kind = nkBool) then
      begin
        MemoMessage('TypeError for transposable expected nkBool got ' + JSONKindToString(Node));
      end;
  end;
'represents_mana':
  begin
    if not(Node.Kind = nkBool) then
      begin
        MemoMessage('TypeError for represents_mana expected nkBool got ' + JSONKindToString(Node));
      end;
  end;
'appears_in_mana_costs':
  begin
    if not(Node.Kind = nkBool) then
      begin
        MemoMessage('TypeError for appears_in_mana_costs expected nkBool got ' + JSONKindToString(Node));
      end;
  end;
'cmc':
  begin
    if not(Node.Kind = nkNumber) then
      begin
        MemoMessage('TypeError for cmc expected nkNumber got ' + JSONKindToString(Node));
      end;
  end;
'funny':
  begin
    if not(Node.Kind = nkBool) then
      begin
        MemoMessage('TypeError for funny expected nkBool got ' + JSONKindToString(Node));
      end;
  end;
'colors':
  begin
    if not(Node.Kind = nkArray) then
      begin
        MemoMessage('TypeError for colors expected nkArray got ' + JSONKindToString(Node));
      end;
  end;
'gatherer_alternates':
  begin
    // Ignore
  end;
}

{ Sets
'object':
  begin
    // Ignore
  end;
'id':
  begin
    if not(Node.Kind = nkString) then
      begin
        MemoMessage('TypeError');
      end;
  end;
'code':
  begin
    if not(Node.Kind = nkString) then
      begin
        MemoMessage('TypeError');
      end;
  end;
'name':
  begin
    if not(Node.Kind = nkString) then
      begin
        MemoMessage('TypeError');
      end;
  end;
'uri':
  begin
    if not(Node.Kind = nkString) then
      begin
        MemoMessage('TypeError');
      end;
  end;
'scryfall_uri':
  begin
    if not(Node.Kind = nkString) then
      begin
        MemoMessage('TypeError');
      end;
  end;
'search_uri':
  begin
    if not(Node.Kind = nkString) then
      begin
        MemoMessage('TypeError');
      end;
  end;
'released_at':
  begin
    if not(Node.Kind = nkString) then
      begin
        MemoMessage('TypeError');
      end;
  end;
'set_type':
  begin
    if not(Node.Kind = nkString) then
      begin
        MemoMessage('TypeError');
      end;
  end;
'card_count':
  begin
    if not(Node.Kind = nkNumber) then
      begin
        MemoMessage('TypeError');
      end;
  end;
'digital':
  begin
    if not(Node.Kind = nkBool) then
      begin
        MemoMessage('TypeError');
      end;
  end;
'nonfoil_only':
  begin
    if not(Node.Kind = nkBool) then
      begin
        MemoMessage('TypeError');
      end;
  end;
'foil_only':
  begin
    if not(Node.Kind = nkBool) then
      begin
        MemoMessage('TypeError');
      end;
  end;
'icon_svg_uri':
  begin
    if not(Node.Kind = nkString) then
      begin
        MemoMessage('TypeError');
      end;
  end;
'tcgplayer_id':
  begin
    if not(Node.Kind = nkNumber) then
      begin
        MemoMessage('TypeError');
      end;
  end;
'block_code':
  begin
    if not(Node.Kind = nkString) then
      begin
        MemoMessage('TypeError');
      end;
  end;
'block':
  begin
    if not(Node.Kind = nkString) then
      begin
        MemoMessage('TypeError');
      end;
  end;
'parent_set_code':
  begin
    if not(Node.Kind = nkString) then
      begin
        MemoMessage('TypeError');
      end;
  end;
'mtgo_code':
  begin
    if not(Node.Kind = nkString) then
      begin
        MemoMessage('TypeError');
      end;
  end;
'arena_code':
  begin
    if not(Node.Kind = nkString) then
      begin
        MemoMessage('TypeError');
      end;
  end;
}

end.

