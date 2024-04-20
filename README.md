# vpinball-example-table-extracted
Extracted version of the vpinball example table

## Vpinbal example table

The table file can be downloaded from: https://github.com/vpinball/vpinball/blob/master/src/assets/exampleTable.vpx

## How to extract

You need to install [vpxtool](https://github.com/francisdb/vpxtool) first.

```
vpxtool extract exampleTable.vpx
```

This gives you a directory named `exampleTable` next tot he table.

It's possible to modify the extracted files and re-assemble a vpx file:

```
vpxtool assemble exampleTable
```
