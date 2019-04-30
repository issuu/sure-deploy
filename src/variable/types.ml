type name = string

type substitution = string

type msg = string

type value =
  | String of string
  | Variable of name
  | Unset_variable of name * substitution
  | Unset_or_empty_variable of name * substitution
  | Unset_error_variable of name * msg
  | Unset_or_empty_error_variable of name * msg
