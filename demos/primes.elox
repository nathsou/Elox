
class Sieve {
    init() {
        this.primes = [2];
    }

    isPrime(n) {
        if (n < 2) return false;
        if (n == 2) return true;
        if (n % 2 == 0) return false;

        var p = this.primes[0];
        var sieve_size = this.primes.length();

        for (var i = 1; i < sieve_size and p * p <= n; i++) {
            if (n % p == 0) return false;
            p = this.primes[i];
        }

        this.primes.push(n);
        return true;
    }
    
    #str() {
      return this.primes.#str();
    }
}

fun time(fn) {
    var t1 = clock();
    fn();
    return (clock() - t1) * 1000; // ms
}

fun findPrimes() {
    var n = 10000;
    var sieve = Sieve();

    for (var i = 1; i <= n; i++) {
        sieve.isPrime(i);
    }

    print "there are " + sieve.primes.length() + " prime numbers <= " + n + " :";
    print sieve;
}

fun floor(x) {
  return x - x % 1;
}

print "took " + floor(time(findPrimes)) + "ms";