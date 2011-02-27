G: server open 
L: client connects
G: send a list of file paths and timestamp
L: reply with the wanted files
G: send the files compacted in some format, maybe with diff and not the entire file
L: apply file changes and send file list and timestamp
G: request wanted files
L: send the files compacted in some format, maybe with diff and not the entire file
G: apply file changes and wait