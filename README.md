# json2fs

Tool which converts json from file into F# records. There's also library Json2Fs.Core. Both can be found on nuget.org.


## Usage of Json2Fs

Get it from nuget:
```
dotnet tool install json2fs --global --prerelease
```

Run it like this:
```
json2fs WebsiteResponse your/path/to.json dst.fs
```

Where `WebsiteResponse` is the name of the root record (the one you will be deserializing into).

## Usage of Json2Fs.Core

```fs
open Json2Fs.Core

let json = ... // string

let jsonElement = JsonSerializer.Deserialize<JsonElement> json

let dtos = generateFSharpDTOsByJson typename jsonElement // a list of records

let ``F# code`` = fsRecordsToString dtos // this converts it to the actual code
```