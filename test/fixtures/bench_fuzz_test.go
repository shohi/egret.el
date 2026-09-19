// Fixture for egret's benchmark/fuzz detection tests
// (test/egret-bench-fuzz-test.el).  Parsed only, not runnable as-is.
package fixtures

import "testing"

func BenchmarkOne(b *testing.B) {
	// egret-marker: bench-one
	for i := 0; i < b.N; i++ {
	}
}

func BenchmarkTwo(b *testing.B) {
	// egret-marker: bench-two
	for i := 0; i < b.N; i++ {
	}
}

func FuzzThing(f *testing.F) {
	f.Fuzz(func(t *testing.T, in []byte) {
		// egret-marker: fuzz-thing
		_ = in
	})
}
