# ComposedSet

Use this library on a dataset when:

- Each data element in a dataset can be further split into parts
- Each part is typically repeated in other data elements
- There is not a constant need to compose back to the original data representation
- Performance on operations in the decomposed state matters
- Memory footprint is important and many of the parts are reused

## Example : Folder structure
Assume we have the following paths
```
data/example/code/myfile.cpp
data/production/code/some.cpp
```
We will then split using '/', '.' and put each unique part in a indexable collection :
```
[0:data | 1:/ | 2:example | 3:code | 4:myfile | 5:. | 6:cpp | 7:production | 8:some]
```
The two paths can now be represented as indicies into the shared collection of parts:
```
[0,1,2,1,3,1,4,5,6]
[0,1,7,1,3,1,8,5,6]
```
Now operation like 'find all .cpp files in the data folder' becomes a lot faster than comparing the raw strings
because that will just be checking if the indicies they start with [0,1] (data/) and end with [5,6] (.cpp).

Concatination and substring operations also become significantly faster.


## Build Status

Mono | .NET
---- | ----
[![Mono CI Build Status](https://img.shields.io/travis/kjems/ComposedSet/master.svg)](https://travis-ci.org/kjems/ComposedSet) | [![.NET Build Status](https://img.shields.io/appveyor/ci/kjems/ComposedSet/master.svg)](https://ci.appveyor.com/project/kjems/ComposedSet)

## Maintainer(s)

- [@kjems](https://github.com/kjems)
