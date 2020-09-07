unit SVGUtils;

// https://github.com/bgrabitmap/bgrabitmap.git
// Compile BGRABitmapPack
// Install BGRAControls

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BGRABitmap, BGRASVG;

procedure RasterizeSVG(FileName: String; TextureSize: Integer; SVGContent: TStream);
procedure RasterizeSVG(FileName: String; TextureSize: Integer; SVGContent: String);

implementation

procedure RasterizeSVG(FileName: String; TextureSize: Integer; SVGContent: TStream);
var
  bmp: TBGRABitmap;
  svg: TBGRASVG;
begin
  bmp:= TBGRABitmap.Create;
  try
    svg:= TBGRASVG.Create(SVGContent);
    try
      if (TextureSize > 0) then
        begin
          bmp.SetSize(TextureSize, TextureSize);
          svg.StretchDraw(bmp.Canvas2D, 0,0, TextureSize, TextureSize);
          bmp.SaveToFile(FileName);
        end;
    finally
     FreeAndNil(svg);
    end;
  finally
    FreeAndNil(bmp);
  end;
end;

procedure RasterizeSVG(FileName: String; TextureSize: Integer; SVGContent: String);
var
  bmp: TBGRABitmap;
  svg: TBGRASVG;
begin
  bmp:= TBGRABitmap.Create;
  try
    svg:= TBGRASVG.CreateFromString(SVGContent);
    try
      if (TextureSize > 0) then
        begin
          bmp.SetSize(TextureSize, TextureSize);
//          bmp.Fill(0);
          svg.StretchDraw(bmp.Canvas2D, 0,0, TextureSize, TextureSize);
          bmp.SaveToFile(FileName);
        end;
    finally
     FreeAndNil(svg);
    end;
  finally
    FreeAndNil(bmp);
  end;
end;

end.

