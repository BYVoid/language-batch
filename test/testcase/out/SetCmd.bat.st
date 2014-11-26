Program
  { program_stmts =
      [ Set
          { stmt_set =
              Unparsed
                { unparsed_string = "var1=Hello world"
                , unparsed_pos =
                    LP { lpLine = 1 , lpColumn = 1 , lpStartByte = 0 , lpLength = 20 }
                }
          , stmt_pos =
              LP { lpLine = 1 , lpColumn = 1 , lpStartByte = 0 , lpLength = 20 }
          }
      , Set
          { stmt_set =
              Unparsed
                { unparsed_string = "var2=\"Hello world 2\""
                , unparsed_pos =
                    LP { lpLine = 2 , lpColumn = 1 , lpStartByte = 21 , lpLength = 24 }
                }
          , stmt_pos =
              LP { lpLine = 2 , lpColumn = 1 , lpStartByte = 21 , lpLength = 24 }
          }
      ]
  , program_pos =
      LP { lpLine = 1 , lpColumn = 1 , lpStartByte = 0 , lpLength = 45 }
  }
