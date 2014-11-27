Program
  { program_stmts =
      [ Set
          { stmt_set =
              Unparsed
                { unparsed_string = "var00=()"
                , unparsed_pos =
                    LP { lpLine = 1 , lpColumn = 5 , lpStartByte = 4 , lpLength = 12 }
                }
          , stmt_pos =
              LP { lpLine = 1 , lpColumn = 1 , lpStartByte = 0 , lpLength = 12 }
          }
      , Set
          { stmt_set =
              Unparsed
                { unparsed_string = "var01=^(^)"
                , unparsed_pos =
                    LP { lpLine = 2 , lpColumn = 5 , lpStartByte = 17 , lpLength = 14 }
                }
          , stmt_pos =
              LP { lpLine = 2 , lpColumn = 1 , lpStartByte = 13 , lpLength = 14 }
          }
      , Set
          { stmt_set =
              Unparsed
                { unparsed_string = "\"var02=()\""
                , unparsed_pos =
                    LP { lpLine = 3 , lpColumn = 5 , lpStartByte = 32 , lpLength = 14 }
                }
          , stmt_pos =
              LP { lpLine = 3 , lpColumn = 1 , lpStartByte = 28 , lpLength = 14 }
          }
      , Set
          { stmt_set =
              Unparsed
                { unparsed_string = "\"var03=^(^)\""
                , unparsed_pos =
                    LP { lpLine = 4 , lpColumn = 5 , lpStartByte = 47 , lpLength = 16 }
                }
          , stmt_pos =
              LP { lpLine = 4 , lpColumn = 1 , lpStartByte = 43 , lpLength = 16 }
          }
      , Set
          { stmt_set =
              Unparsed
                { unparsed_string = "var04=\"\""
                , unparsed_pos =
                    LP { lpLine = 5 , lpColumn = 5 , lpStartByte = 64 , lpLength = 12 }
                }
          , stmt_pos =
              LP { lpLine = 5 , lpColumn = 1 , lpStartByte = 60 , lpLength = 12 }
          }
      , Set
          { stmt_set =
              Unparsed
                { unparsed_string = "var05=^\"^\""
                , unparsed_pos =
                    LP { lpLine = 6 , lpColumn = 5 , lpStartByte = 77 , lpLength = 14 }
                }
          , stmt_pos =
              LP { lpLine = 6 , lpColumn = 1 , lpStartByte = 73 , lpLength = 14 }
          }
      , Set
          { stmt_set =
              Unparsed
                { unparsed_string = "\"var06=\"\"\""
                , unparsed_pos =
                    LP { lpLine = 7 , lpColumn = 5 , lpStartByte = 92 , lpLength = 14 }
                }
          , stmt_pos =
              LP { lpLine = 7 , lpColumn = 1 , lpStartByte = 88 , lpLength = 14 }
          }
      , Set
          { stmt_set =
              Unparsed
                { unparsed_string = "\"var07=^\"^\"\""
                , unparsed_pos =
                    LP
                      { lpLine = 8 , lpColumn = 5 , lpStartByte = 107 , lpLength = 16 }
                }
          , stmt_pos =
              LP
                { lpLine = 8 , lpColumn = 1 , lpStartByte = 103 , lpLength = 16 }
          }
      , Set
          { stmt_set =
              Unparsed
                { unparsed_string = "var08=^^"
                , unparsed_pos =
                    LP
                      { lpLine = 9 , lpColumn = 5 , lpStartByte = 124 , lpLength = 12 }
                }
          , stmt_pos =
              LP
                { lpLine = 9 , lpColumn = 1 , lpStartByte = 120 , lpLength = 12 }
          }
      , Set
          { stmt_set =
              Unparsed
                { unparsed_string = "\"var09=^\""
                , unparsed_pos =
                    LP
                      { lpLine = 10 , lpColumn = 5 , lpStartByte = 137 , lpLength = 13 }
                }
          , stmt_pos =
              LP
                { lpLine = 10 , lpColumn = 1 , lpStartByte = 133 , lpLength = 13 }
          }
      , Set
          { stmt_set =
              Unparsed
                { unparsed_string = "\"var10=^^\""
                , unparsed_pos =
                    LP
                      { lpLine = 11 , lpColumn = 5 , lpStartByte = 151 , lpLength = 14 }
                }
          , stmt_pos =
              LP
                { lpLine = 11 , lpColumn = 1 , lpStartByte = 147 , lpLength = 14 }
          }
      , Set
          { stmt_set =
              Unparsed
                { unparsed_string = "var11=^&"
                , unparsed_pos =
                    LP
                      { lpLine = 12 , lpColumn = 5 , lpStartByte = 166 , lpLength = 12 }
                }
          , stmt_pos =
              LP
                { lpLine = 12 , lpColumn = 1 , lpStartByte = 162 , lpLength = 12 }
          }
      , Set
          { stmt_set =
              Unparsed
                { unparsed_string = "\"var12=&\""
                , unparsed_pos =
                    LP
                      { lpLine = 13 , lpColumn = 5 , lpStartByte = 179 , lpLength = 13 }
                }
          , stmt_pos =
              LP
                { lpLine = 13 , lpColumn = 1 , lpStartByte = 175 , lpLength = 13 }
          }
      , Set
          { stmt_set =
              Unparsed
                { unparsed_string = "\"var13=^&\""
                , unparsed_pos =
                    LP
                      { lpLine = 14 , lpColumn = 5 , lpStartByte = 193 , lpLength = 14 }
                }
          , stmt_pos =
              LP
                { lpLine = 14 , lpColumn = 1 , lpStartByte = 189 , lpLength = 14 }
          }
      , Set
          { stmt_set =
              Unparsed
                { unparsed_string = "var14=^|"
                , unparsed_pos =
                    LP
                      { lpLine = 15 , lpColumn = 5 , lpStartByte = 208 , lpLength = 12 }
                }
          , stmt_pos =
              LP
                { lpLine = 15 , lpColumn = 1 , lpStartByte = 204 , lpLength = 12 }
          }
      , Set
          { stmt_set =
              Unparsed
                { unparsed_string = "\"var15=|\""
                , unparsed_pos =
                    LP
                      { lpLine = 16 , lpColumn = 5 , lpStartByte = 221 , lpLength = 13 }
                }
          , stmt_pos =
              LP
                { lpLine = 16 , lpColumn = 1 , lpStartByte = 217 , lpLength = 13 }
          }
      , Set
          { stmt_set =
              Unparsed
                { unparsed_string = "\"var16=^|\""
                , unparsed_pos =
                    LP
                      { lpLine = 17 , lpColumn = 5 , lpStartByte = 235 , lpLength = 14 }
                }
          , stmt_pos =
              LP
                { lpLine = 17 , lpColumn = 1 , lpStartByte = 231 , lpLength = 14 }
          }
      , Set
          { stmt_set =
              Unparsed
                { unparsed_string = "var17=^<^>"
                , unparsed_pos =
                    LP
                      { lpLine = 18 , lpColumn = 5 , lpStartByte = 250 , lpLength = 14 }
                }
          , stmt_pos =
              LP
                { lpLine = 18 , lpColumn = 1 , lpStartByte = 246 , lpLength = 14 }
          }
      , Set
          { stmt_set =
              Unparsed
                { unparsed_string = "\"var18=<>\""
                , unparsed_pos =
                    LP
                      { lpLine = 19 , lpColumn = 5 , lpStartByte = 265 , lpLength = 14 }
                }
          , stmt_pos =
              LP
                { lpLine = 19 , lpColumn = 1 , lpStartByte = 261 , lpLength = 14 }
          }
      , Set
          { stmt_set =
              Unparsed
                { unparsed_string = "\"var19=^<^>\""
                , unparsed_pos =
                    LP
                      { lpLine = 20 , lpColumn = 5 , lpStartByte = 280 , lpLength = 16 }
                }
          , stmt_pos =
              LP
                { lpLine = 20 , lpColumn = 1 , lpStartByte = 276 , lpLength = 16 }
          }
      , Set
          { stmt_set =
              Unparsed
                { unparsed_string = "var"
                , unparsed_pos =
                    LP
                      { lpLine = 21 , lpColumn = 5 , lpStartByte = 297 , lpLength = 7 }
                }
          , stmt_pos =
              LP
                { lpLine = 21 , lpColumn = 1 , lpStartByte = 293 , lpLength = 7 }
          }
      ]
  , program_pos =
      LP { lpLine = 1 , lpColumn = 1 , lpStartByte = 0 , lpLength = 300 }
  }
