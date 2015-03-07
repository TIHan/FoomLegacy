module Foom.Shared.Wad.UnpickleWad

open System
open System.Numerics
open FSharp.LitePickler.Core
open FSharp.LitePickler.Unpickle

let inline u_arrayi n (p: int -> Unpickle<'a>) =
    fun stream ->
        match n with
        | 0 -> [||]
        | _ -> Array.init n (fun i -> p i stream)

let inline fixedToSingle x = (single x / 65536.f)

let u_header : Unpickle<Header> =
    u_pipe3 (u_string 4) u_int32 u_int32 <|
    fun id lumpCount lumpOffset ->
        { IsPwad = if id = "IWAD" then false else true
          LumpCount = lumpCount
          LumpOffset = lumpOffset }

let u_lumpHeader : Unpickle<LumpHeader> =
    u_pipe3 u_int32 u_int32 (u_string 8) <| fun offset size name -> { Offset = offset; Size = size; Name = name.Trim().Trim('\000') }

let u_lumpHeaders count offset : Unpickle<LumpHeader []> =
    u_skipBytes offset >>. u_array count u_lumpHeader

let u_wad : Unpickle<Wad> =
    u_lookAhead u_header >>= fun header ->
        (u_lookAhead <| (u_lumpHeaders header.LumpCount (int64 header.LumpOffset)) |>> (fun lumpHeaders -> { Header = header; LumpHeaders = lumpHeaders }))

[<Literal>]
let doomThingSize = 10
[<Literal>]
let hexenThingSize = 20
let u_thing format : Unpickle<ThingData> =
    match format with
    | ThingDataFormat.Doom ->
        u_pipe5 u_int16 u_int16 u_int16 u_int16 u_int16 <|
        fun x y angle _ flags ->
            ThingData.Doom { X = int x; Y = int y; Angle = int angle; Flags = enum<DoomThingDataFlags> (int flags) }
    | _ -> failwith "Not supported."

let u_things format count offset : Unpickle<ThingData []> =
    u_skipBytes offset >>. u_array count (u_thing format)

let u_lumpThings format size offset : Unpickle<LumpThings> =
    match format with
    | ThingDataFormat.Doom ->
        u_lookAhead (u_things format (size / doomThingSize) offset) |>> fun things -> { Things = things }
    | _ -> failwith "Not supported."

[<Literal>]
let vertexSize = 4
let u_vertex : Unpickle<Vector2> =
    u_pipe2 u_int16 u_int16 <|
    fun x y -> Vector2 (fixedToSingle x, fixedToSingle y)

let u_vertices count offset : Unpickle<Vector2 []> =
    u_skipBytes offset >>. u_array count u_vertex

let u_lumpVertices size offset : Unpickle<LumpVertices> =
    u_lookAhead (u_vertices (size / vertexSize) offset) |>> fun vertices -> { Vertices = vertices }

[<Literal>]
let sidedefSize = 30
let u_sidedef : Unpickle<SidedefData> =
    u_pipe6 u_int16 u_int16 (u_string 8) (u_string 8) (u_string 8) u_int16 <|
    fun offsetX offsetY upperTexName lowerTexName middleTexName sectorNumber ->
        { OffsetX = int offsetX
          OffsetY = int offsetY
          UpperTextureName = upperTexName.Trim().Trim('\000')
          LowerTextureName = lowerTexName.Trim().Trim('\000')
          MiddleTextureName = middleTexName.Trim().Trim('\000')
          SectorNumber = int sectorNumber }

let u_sidedefs count offset : Unpickle<SidedefData []> =
    u_skipBytes offset >>. u_array count u_sidedef

let u_lumpSidedefs size offset : Unpickle<LumpSidedefs> =
    u_lookAhead (u_sidedefs (size / sidedefSize) offset) |>> fun sidedefs -> { Sidedefs = sidedefs }

[<Literal>]
let linedefSize = 14
let u_linedef (vertices: Vector2 []) (sidedefs: SidedefData []) : Unpickle<LinedefData> =
    u_pipe7 u_uint16 u_uint16 u_int16 u_int16 u_int16 u_uint16 u_uint16 <|
    fun startVertex endVertex flags specialType sectorTag rightSidedef leftSidedef ->
        let data =
            { Flags = enum<LinedefDataFlags> (int flags)
              SpecialType = int specialType
              SectorTag = int sectorTag }
        let f = match int rightSidedef with | n when n <> 65535 -> Some sidedefs.[n] | _ -> None
        let b = match int leftSidedef with | n when n <> 65535 -> Some sidedefs.[n] | _ -> None
        LinedefData.Doom (
            vertices.[int startVertex],
            vertices.[int endVertex],
            f,
            b,
            data)

let u_linedefs (vertices: Vector2 []) (sidedefs: SidedefData []) count offset : Unpickle<LinedefData []> =
    u_skipBytes offset >>. u_array count (u_linedef vertices sidedefs)
        
let u_lumpLinedefs (vertices: Vector2 []) (sidedefs: SidedefData []) size offset : Unpickle<LumpLinedefs> =
    u_lookAhead (u_linedefs vertices sidedefs (size / linedefSize) offset) |>> fun linedefs -> { Linedefs = linedefs }

[<Literal>]
let sectorSize = 26
let u_sector (linedefs: LinedefData []) (i: int) : Unpickle<SectorData> =
    u_pipe7 u_int16 u_int16 (u_string 8) (u_string 8) u_int16 u_int16 u_int16 <|
    fun floorHeight ceilingHeight floorTexName ceilingTexName lightLevel typ tag ->
        { FloorHeight = int floorHeight
          CeilingHeight = int ceilingHeight
          FloorTextureName = floorTexName.Trim().Trim('\000')
          CeilingTextureName = ceilingTexName.Trim().Trim('\000')
          LightLevel = int lightLevel
          Type = enum<SectorDataType> (int typ)
          Tag = int tag
          Linedefs = 
            linedefs
            |> Array.filter (function
                | LinedefData.Doom (_, _, f, b, _) -> 
                    match f, b with
                    | Some f, Some b -> f.SectorNumber = i || b.SectorNumber = i
                    | Some f, _ -> f.SectorNumber = i
                    | _, Some b -> b.SectorNumber = i
                    | _ -> false) }

let u_sectors (linedefs: LinedefData []) count offset : Unpickle<SectorData []> =
    u_skipBytes offset >>. u_arrayi count (u_sector linedefs)

let u_lumpSectors (linedefs: LinedefData []) size offset : Unpickle<LumpSectors> =
    u_lookAhead (u_sectors linedefs (size / sectorSize) offset) |>> fun sectors -> { Sectors = sectors }