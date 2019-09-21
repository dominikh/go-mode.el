module my/thing

// comment
go 1.12

require other/thing v1.0.2
require new/thing/v2 v2.3.4
exclude old/thing v1.2.3
replace bad/thing v1.4.5 => good/thing v1.4.5

require (
	// comment inside block
	new/thing v2.3.4
	old/thing v1.2.3
)

replace (
	bad/thing v1.4.5 => good/thing v1.4.5
)
