use "lexer.sml";

val args = CommandLine.arguments();

fun main (file_name::[]) = (
    pp_list (tokenizeString (TextIO.inputAll (TextIO.openIn file_name)));
    OS.Process.exit OS.Process.success
  )
  | main _ = (
    print "Usage: <sml cli.sml || sml_c> <c program path>";
    OS.Process.exit OS.Process.failure
  );

val _ = main (CommandLine.arguments());

