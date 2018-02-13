type name = string
type substitution = string
type msg = string

type value =
  | String of string
  | Variable of name
  | Empty_variable of name * substitution
  | Unset_variable of name * substitution
  | Empty_error_variable of name * msg
  | Unset_error_variable of name * msg
