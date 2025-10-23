from dwave.system import DWaveSampler, EmbeddingComposite

chainstrength = 1
numruns = 1000


def main():
    h = {}
    J = {}

    J[('a', 'b')] = 3.0
    J[('a', 'b')] = 2.0
    J[('a', 'b')] = 6.0

    sampler = EmbeddingComposite(DWaveSampler())
    sampleset = sampler.sample_ising(h, J, chain_strength=chainstrength, num_reads=numruns, label="D-Wave Example Ising")
    print(sampleset)


if __name__ == '__main__':
    main()

