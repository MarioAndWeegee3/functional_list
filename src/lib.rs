mod list;

pub use list::*;

#[macro_export]
/// Builds a `List`
macro_rules! list {
    () => {
        {
            use $crate::list::List;
            List::nil()
        }
    };
    ($head: expr $(, $tail: expr)* $(,)?) => {
        {
            use $crate::list::List;
            List::cons(
                $head,
                $crate::list![$($tail),*]
            )
        }
    };
}

#[macro_export]
/// Builds a `ListMap`
macro_rules! list_map {
    ($($key: expr => $value: expr),* $(,)?) => {
        $crate::list![$(($key, $value)),*]
    };
}

#[cfg(test)]
mod tests {
    use crate::{List};

    #[test]
    fn macro_test() {
        let list = list![2, 3];
        assert_eq!(
            list,
            List::cons(
                2,
                List::cons(
                    3,
                    List::nil()
                )
            )
        )
    }

    #[test]
    fn fold_test() {
        let list = list!(
            2, 3, 4, 5
        );

        let string = list.fold(String::new(), |acc, x| format!("{}{}", acc, x));

        assert_eq!(
            string,
            "2345"
        )
    }

    #[test]
    fn rev_test() {
        let list = list![2, 3, 4];
        let rev_list = list.reversed();
        assert_eq!(
            rev_list,
            list![4, 3, 2]
        );
    }

    #[test]
    fn append_test() {
        let list1 = list![2, 3];
        let list2 = list![4, 5];

        let list = list1.append(&list2).cloned();

        assert_eq!(
            list,
            list![2, 3, 4, 5]
        )
    }

    #[test]
    fn map_test() {
        let list = list![2, 3, 4];

        let list = list.map(|&x| x - 1);

        assert_eq!(list, list![1, 2, 3])
    }

    #[test]
    fn join_test() {
        let list = list![
            list![1, 2, 3],
            list![4, 5, 6],
            list![7, 8, 9],
        ];

        let list = list.join().cloned();

        assert_eq!(
            list,
            list![1, 2, 3, 4, 5, 6, 7, 8, 9]
        )
    }

    #[test]
    fn take_test() {
        let list = list![1, 2, 3, 4];

        let sub_list = list.take(2).cloned();

        assert_eq!(sub_list, list![1, 2]);
    }

    #[test]
    fn take_while_test() {
        let list = list![1, 2, 3, 4];

        let sub_list = list.take_while(|&x| x < 3).cloned();

        assert_eq!(sub_list, list![1, 2]);
    }

    #[test]
    fn zip_test() {
        let a_list = list!["a", "b", "c"];
        let b_list = list![1, 2, 3];

        let list = a_list
            .zip(&b_list)
            .map(|(k, v)| (**k, **v));

        assert_eq!(
            list,
            list![
                ("a", 1),
                ("b", 2),
                ("c", 3),
            ]
        );
    }

    #[test]
    fn map_macro_test() {
        let map = list_map![
            "a" => 2,
            "b" => 3,
            "c" => 7,
        ];

        assert_eq!(
            map,
            list![
                ("a", 2),
                ("b", 3),
                ("c", 7),
            ]
        );

        let map = map.insert("d", 45);

        assert_eq!(map.get("d"), Some(&45));

        let map = map.insert("d", 4);

        assert_eq!(map.get("d"), Some(&4));

        assert_eq!(
            map,
            list_map![
                "d" => 4,
                "d" => 45,
                "a" => 2,
                "b" => 3,
                "c" => 7,
            ]
        )
    }

    mod interpret {
        use crate::{ListMap};
        use std::num::Wrapping;
        use std::rc::Rc;
        use std::convert::TryInto;
        use std::fmt::{Debug, Formatter};
        use std::fmt;

        #[derive(Clone, Debug, Eq, PartialEq)]
        enum Value {
            Int(Wrapping<i32>),
            Closure(Rc<Closure>),
        }

        impl From<i32> for Value {
            fn from(v: i32) -> Self {
                Value::Int(Wrapping(v))
            }
        }

        impl From<Closure> for Value {
            fn from(c: Closure) -> Self {
                Value::Closure(Rc::new(c))
            }
        }

        impl TryInto<Wrapping<i32>> for Value {
            type Error = String;

            fn try_into(self) -> Result<Wrapping<i32>, Self::Error> {
                match self {
                    Value::Int(i) => {
                        Ok(i)
                    }
                    v => Err(format!("{:?} is not an int", v))
                }
            }
        }

        impl From<Wrapping<i32>> for Value {
            fn from(i: Wrapping<i32>) -> Self {
                Value::Int(i)
            }
        }

        impl TryInto<Rc<Closure>> for Value {
            type Error = String;

            fn try_into(self) -> Result<Rc<Closure>, Self::Error> {
                match self {
                    Value::Closure(c) => Ok(c),
                    v => Err(format!("{:?} is not a function", v))
                }
            }
        }

        #[derive(Eq, PartialEq)]
        struct Closure {
            env: Env,
            param: Rc<String>,
            body: Rc<Expr>,
        }

        impl Closure {
            fn call(&self, arg: Value) -> Result<Value, String> {
                let env = self.env.insert(self.param.clone(), arg);
                eval(&self.body, env)
            }
        }

        impl Debug for Closure {
            fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
                write!(f, "<function {:?} -> {:?}>", self.param, self.body)
            }
        }

        #[derive(Debug, Clone, Eq, PartialEq)]
        enum Expr {
            Lit(Value),
            Pre(&'static str, Rc<Expr>),
            Bin(Rc<Expr>, &'static str, Rc<Expr>),
            Let(Rc<String>, Rc<Expr>, Rc<Expr>),
            Var(Rc<String>),
            Lambda {
                param: Rc<String>,
                body: Rc<Expr>,
            },
            Call(Rc<Expr>, Rc<Expr>),
        }

        type Env = ListMap<Rc<String>, Value>;

        fn eval(expr: &Expr, env: Env) -> Result<Value, String> {
            match expr {
                Expr::Lit(v) => Ok(v.clone()),
                Expr::Pre(op, v) => {
                    let v = eval(v, env)?;
                    match *op {
                        "-" => {
                            let v: Wrapping<i32> = v.try_into()?;
                            Ok((-v).into())
                        }
                        "~" => {
                            let v: Wrapping<i32> = v.try_into()?;
                            Ok((!v).into())
                        }
                        "!" => Ok(
                            if v == Value::Int(Wrapping(0)) {
                                1.into()
                            } else {
                                0.into()
                            }
                        ),
                        _ => Err(format!("Unsupported prefix operator {:?}", op))
                    }
                }
                Expr::Bin(left, op, right) => {
                    let left: Wrapping<i32> = eval(left, env.clone())?.try_into()?;
                    let right: Wrapping<i32> = eval(right, env)?.try_into()?;

                    match *op {
                        "+" => Ok((left + right).into()),
                        "-" => Ok((left - right).into()),
                        "*" => Ok((left * right).into()),
                        "/" => Ok((left / right).into()),
                        "%" => Ok((left % right).into()),
                        "&" => Ok((left & right).into()),
                        "|" => Ok((left | right).into()),
                        "^" => Ok((left ^ right).into()),
                        _ => Err(format!("Unsupported binary operator {:?}", op))
                    }
                }
                Expr::Let(name, value, next) => {
                    let value = eval(value, env.clone())?;
                    let env = env.insert(name.clone(), value);

                    eval(next, env)
                }
                Expr::Var(name) => {
                    env.get(name)
                        .ok_or(format!("Variable with name {:?} not found", name))
                        .map(|v| v.clone())
                }
                Expr::Lambda { param, body } => {
                    let param = param.clone();
                    let body = body.clone();

                    let closure = Closure {
                        param,
                        body,
                        env,
                    };

                    Ok(closure.into())
                }
                Expr::Call(callee, arg) => {
                    let callee = eval(callee, env.clone())?;
                    let arg = eval(arg, env)?;

                    let closure: Rc<Closure> = callee.try_into()?;

                    closure.call(arg)
                }
            }
        }

        #[test]
        fn lit_test() {
            let expr = Expr::Lit(3.into());

            let value = eval(&expr, Default::default()).unwrap();

            assert_eq!(
                value,
                3.into()
            )
        }

        #[test]
        fn pre_test() {
            let expr = Expr::Pre("!", Expr::Lit(4.into()).into());

            let value = eval(&expr, Default::default()).unwrap();

            assert_eq!(
                value,
                0.into()
            )
        }

        #[test]
        fn bin_test() {
            let expr = Expr::Bin(
                Expr::Lit(2.into()).into(),
                "+",
                Expr::Lit(3.into()).into(),
            );

            let value = eval(&expr, Default::default()).unwrap();

            assert_eq!(
                value,
                5.into()
            )
        }

        #[test]
        fn let_test() {
            let x: Rc<String> = Rc::new("x".into());

            let expr = Expr::Let(
                x.clone(),
                Expr::Lit(2.into()).into(),
                Expr::Bin(
                    Expr::Var(x.clone()).into(),
                    "*",
                    Expr::Var(x).into(),
                ).into(),
            );

            let value = eval(&expr, Default::default()).unwrap();

            assert_eq!(
                value,
                4.into()
            )
        }

        #[test]
        fn closure_test() {
            let id = Rc::new(String::from("id"));
            let x = Rc::new(String::from("x"));
            let x_expr = Rc::new(Expr::Var(x.clone()));

            let expr = Expr::Let(
                x.clone(),
                Expr::Lit(4.into()).into(),
                Expr::Let(
                    id.clone(),
                    Expr::Lambda {
                        param: x.clone(),
                        body: x_expr.clone(),
                    }.into(),
                    Expr::Call(
                        Expr::Var(id.clone()).into(),
                        x_expr.clone(),
                    ).into()
                ).into()
            );

            let value = eval(&expr, Default::default()).unwrap();

            assert_eq!(
                value,
                4.into()
            )
        }
    }
}