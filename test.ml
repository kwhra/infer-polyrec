open Syntax
open EnvU
open EnvD
open Subst

(* expvar *)
let	a	=	"a"
let	b	=	"b"
let	c	=	"c"
let	d	=	"d"
let	e	=	"e"
let	f	=	"f"
let	g	=	"g"
let	h	=	"h"
let	i	=	"i"
let	j	=	"j"
let	k	=	"k"
let	l	=	"l"
let	m	=	"m"
let	n	=	"n"
let	o	=	"o"
let	p	=	"p"
let	q	=	"q"
let	r	=	"r"
let	s	=	"s"
let	t	=	"t"
let	u	=	"u"
let	v	=	"v"
let	w	=	"w"
let	x	=	"x"
let	y	=	"y"
let	z	=	"z"

let	expA	=	ExpVar	a
let	expB	=	ExpVar	b
let	expC	=	ExpVar	c
let	expD	=	ExpVar	d
let	expE	=	ExpVar	e
let	expF	=	ExpVar	f
let	expG	=	ExpVar	g
let	expH	=	ExpVar	h
let	expI	=	ExpVar	i
let	expJ	=	ExpVar	j
let	expK	=	ExpVar	k
let	expL	=	ExpVar	l
let	expM	=	ExpVar	m
let	expN	=	ExpVar	n
let	expO	=	ExpVar	o
let	expP	=	ExpVar	p
let	expQ	=	ExpVar	q
let	expR	=	ExpVar	r
let	expS	=	ExpVar	s
let	expT	=	ExpVar	t
let	expU	=	ExpVar	u
let	expV	=	ExpVar	v
let	expW	=	ExpVar	w
let	expX	=	ExpVar	x
let	expY	=	ExpVar	y
let	expZ	=	ExpVar	z

let	tyA	=	TyVar	0
let	tyB	=	TyVar	1
let	tyC	=	TyVar	2
let	tyD	=	TyVar	3
let	tyE	=	TyVar	4
let	tyF	=	TyVar	5
let	tyG	=	TyVar	6
let	tyH	=	TyVar	7
let	tyI	=	TyVar	8
let	tyJ	=	TyVar	9
let	tyK	=	TyVar	10
let	tyL	=	TyVar	11
let	tyM	=	TyVar	12
let	tyN	=	TyVar	13
let	tyO	=	TyVar	14
let	tyP	=	TyVar	15
let	tyQ	=	TyVar	16
let	tyR	=	TyVar	17
let	tyS	=	TyVar	18
let	tyT	=	TyVar	19
let	tyU	=	TyVar	20
let	tyV	=	TyVar	21
let	tyW	=	TyVar	22
let	tyX	=	TyVar	23
let	tyY	=	TyVar	24
let	tyZ	=	TyVar	25

(* example on paper *)
(* rec{f = \gy.ifc false y g(fgy)} *)
let appFG = ExpApp(expF, expG)
let ex0''' = ExpApp(expG, ExpApp(appFG, expY))
let ex0'' = ExpApp(ExpApp(ExpApp(ExpCon Ifc, ExpCon Bool), expY), ex0''')
let ex0' = ExpAbs(g, ExpAbs(y, ex0''))
let ex0 = ExpRec(f, ex0')

(* rec{f = \ghy.ifc false y g(fgh(fhgy))} *)
let appFH = ExpApp(expF, expH)
let ex1''' = ExpApp(expG, ExpApp(ExpApp(appFG, expH), ExpApp(ExpApp(appFH, expG), expY)))
let ex1'' = ExpApp(ExpApp(ExpApp(ExpCon Ifc, ExpCon Bool), expY), ex1''')
let ex1' = ExpAbs(g, ExpAbs(h, ExpAbs(y, ex1'')))
let ex1 = ExpRec(f, ex1')

(* rec{f = \gh1h2y.ifc false y (g(fgh1h2(fh1gh2(fh1h2g y))))} *)
let h' = "h'"
let expH' = ExpVar h'
let appFH = ExpApp(expF, expH)

let ex2''' = ExpApp(expG, ExpApp(ExpApp(ExpApp(appFG, expH), expH'), ExpApp(ExpApp(ExpApp(appFH, expG), expH'), ExpApp(ExpApp(ExpApp(appFH, expH'), expG), expY))))
let ex2'' = ExpApp(ExpApp(ExpApp(ExpCon Ifc, ExpCon Bool), expY), ex2''')
let ex2' = ExpAbs(g, ExpAbs(h, ExpAbs(h', ExpAbs(y, ex2''))))
let ex2 = ExpRec(f, ex2')

(* rec{f = \gh1h2h3y.ifc false y (g(fgh1h2h3(fh1gh2h3(fh1h2gh3(fh1h2h3g y)))))} *)
let h'' = "h''"
let expH'' = ExpVar h''

let ex3''' = ExpApp(expG, ExpApp(ExpApp(ExpApp(ExpApp(appFG, expH), expH'), expH''), ExpApp(ExpApp(ExpApp(ExpApp(appFH, expG), expH'), expH''), ExpApp(ExpApp(ExpApp(ExpApp(appFH, expH'), expG), expH''), ExpApp(ExpApp(ExpApp(ExpApp(appFH, expH'), expH''), expG), expY)))))
let ex3'' = ExpApp(ExpApp(ExpApp(ExpCon Ifc, ExpCon Bool), expY), ex3''')
let ex3' = ExpAbs(g, ExpAbs(h, ExpAbs(h', ExpAbs(h'', ExpAbs(y, ex3'')))))
let ex3 = ExpRec(f, ex3')

let f' = "f'"
let expF' = ExpVar f'
let exp17true = ExpApp(expG, expX)
let exp17false11 = ExpAbs(y, ExpAbs(h, ExpApp(expG, ExpApp(expH, expY))))
let exp17false1 = ExpApp(ExpApp(expF, expF'), exp17false11)
let exp17false = ExpApp(ExpApp(ExpApp(exp17false1, ExpCon Int), ExpVar x), ExpVar f')
let exp17'' = ExpApp(ExpApp(ExpApp(ExpCon Ifc, ExpEq(ExpVar n, ExpCon Int)), exp17true), exp17false)
let exp17' = ExpAbs(f', ExpAbs(g, ExpAbs(n, ExpAbs(x, exp17''))))
let exp17 = ExpRec(f, exp17')

let initD = EnvD.empty