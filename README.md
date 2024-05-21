# huffman
A simple implementation of the huffman compression algorithm in Haskell.

This algorithm was written primarily with portuguese terms and functions names, but each function role should still be very straightforward. Here is a rundown of the main methods:

- construirArvore: receives a list of characters and their frequency on a string and builds the tree required to apply the Huffman algorithm over this tree.
- codificar: receives a string and the frequency tree and returns the compressed string.
- decodificar: receives compressed string and its frequency tree and returns the decompressed string.
- compactaArquivo: it will read a file, compress it and save both the compressed content and frequency tree to a file called teste.txt
- lerArquivoCompactado: it will read a decompress a file compressed by this algorithm
