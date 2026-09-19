// Fixture for egret's tree-sitter detection tests (test/egret-detect-test.el).
// Not a runnable test suite on its own (no go.mod here) -- parsed only.
package fixtures

import "testing"

// egret-marker: plain
func TestPlain(t *testing.T) {
	t.Log("plain")
}

func TestTable(t *testing.T) {
	tests := []struct {
		name string
		want int
	}{
		// egret-marker: case-one
		{name: "case one", want: 1},
		// egret-marker: case-two
		{name: "case two", want: 2},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			_ = tt.want
		})
	}
}

type FooSuite struct {
	// embeds suite.Suite in a real testify suite; omitted here since
	// this fixture is parsed, not compiled.
}

func (s *FooSuite) TestBar() {
	// egret-marker: suite-method
	_ = s
}

func TestFooSuite(t *testing.T) {
	_ = t
}
