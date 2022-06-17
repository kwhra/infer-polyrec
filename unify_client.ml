let testrules = []
let () =
  printdetail (print_string("[print option] 1-string, 2-latex: "); int_of_string(read_line()));
  if (print_option = 1) then print_string(subst_to_string (unify rules))
                        else print_string(subst_to_latex (unify rules))