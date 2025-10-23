# template.py
from qiskit import QuantumCircuit, QuantumRegister, ClassicalRegister, transpile
from qiskit.circuit.library import RZGate, RZZGate, RXXGate, RYYGate
from qiskit.providers.basic_provider import BasicSimulator
from qiskit.visualization import plot_histogram
import numpy as np
import matplotlib.pyplot as plt

gamma = 0.5
beta  = 0.1
n_qubits = 8

trotter_steps = 10
T = 1.0

qr = QuantumRegister(n_qubits)
cr = ClassicalRegister(n_qubits, name='cr')
qc = QuantumCircuit(qr, cr)

is_clique = False

if is_clique:
    for step in range(trotter_steps):
        for j in range(n_qubits):
            for k in range(n_qubits):
                if j != k:
                    qc.append(RXXGate(4 * gamma / trotter_steps), [j, k])
                    qc.append(RYYGate(4 * gamma / trotter_steps), [j, k])
else:
    for i in range(n_qubits):
        qc.h(i)

for step in range(trotter_steps):
    qc.rz(-2 * gamma * -0.125, 3)  # from: [('I', 'a'), ('I', 'b'), ('I', 'c'), ('Z', 'd')]
    qc.rz(-2 * gamma * -0.125, 5)  # from: [('I', 'a'), ('I', 'b'), ('I', 'e'), ('Z', 'f')]
    qc.rz(-2 * gamma * -0.125, 7)  # from: [('I', 'a'), ('I', 'b'), ('I', 'g'), ('Z', 'h')]
    qc.rz(-2 * gamma * -0.25, 2)  # from: [('I', 'a'), ('I', 'b'), ('Z', 'c'), ('I', 'd')]
    qc.rz(-2 * gamma * -0.25, 4)  # from: [('I', 'a'), ('I', 'b'), ('Z', 'e'), ('I', 'f')]
    qc.rz(-2 * gamma * -0.25, 6)  # from: [('I', 'a'), ('I', 'b'), ('Z', 'g'), ('I', 'h')]
    qc.rz(-2 * gamma * 0.125, 1)  # from: [('I', 'a'), ('Z', 'b'), ('I', 'c'), ('I', 'd')]
    qc.rz(-2 * gamma * 0.125, 1)  # from: [('I', 'a'), ('Z', 'b'), ('I', 'c'), ('Z', 'd')]
    qc.rz(-2 * gamma * 0.125, 3)  # from: [('I', 'a'), ('Z', 'b'), ('I', 'c'), ('Z', 'd')]
    qc.rz(-2 * gamma * 0.125, 1)  # from: [('I', 'a'), ('Z', 'b'), ('I', 'e'), ('I', 'f')]
    qc.rz(-2 * gamma * 0.125, 1)  # from: [('I', 'a'), ('Z', 'b'), ('I', 'e'), ('Z', 'f')]
    qc.rz(-2 * gamma * 0.125, 5)  # from: [('I', 'a'), ('Z', 'b'), ('I', 'e'), ('Z', 'f')]
    qc.rz(-2 * gamma * 0.125, 1)  # from: [('I', 'a'), ('Z', 'b'), ('I', 'g'), ('I', 'h')]
    qc.rz(-2 * gamma * 0.125, 1)  # from: [('I', 'a'), ('Z', 'b'), ('I', 'g'), ('Z', 'h')]
    qc.rz(-2 * gamma * 0.125, 7)  # from: [('I', 'a'), ('Z', 'b'), ('I', 'g'), ('Z', 'h')]
    qc.rz(-2 * gamma * 0.125, 5)  # from: [('I', 'c'), ('I', 'd'), ('I', 'e'), ('Z', 'f')]
    qc.rz(-2 * gamma * -0.125, 7)  # from: [('I', 'c'), ('I', 'd'), ('I', 'g'), ('Z', 'h')]
    qc.rz(-2 * gamma * 0.25, 4)  # from: [('I', 'c'), ('I', 'd'), ('Z', 'e'), ('I', 'f')]
    qc.rz(-2 * gamma * -0.25, 6)  # from: [('I', 'c'), ('I', 'd'), ('Z', 'g'), ('I', 'h')]
    qc.rz(-2 * gamma * -0.125, 3)  # from: [('I', 'c'), ('Z', 'd'), ('I', 'e'), ('I', 'f')]
    qc.rz(-2 * gamma * 0.125, 3)  # from: [('I', 'c'), ('Z', 'd'), ('I', 'e'), ('Z', 'f')]
    qc.rz(-2 * gamma * 0.125, 5)  # from: [('I', 'c'), ('Z', 'd'), ('I', 'e'), ('Z', 'f')]
    qc.rz(-2 * gamma * 0.125, 3)  # from: [('I', 'c'), ('Z', 'd'), ('I', 'g'), ('I', 'h')]
    qc.rz(-2 * gamma * 0.125, 3)  # from: [('I', 'c'), ('Z', 'd'), ('I', 'g'), ('Z', 'h')]
    qc.rz(-2 * gamma * 0.125, 7)  # from: [('I', 'c'), ('Z', 'd'), ('I', 'g'), ('Z', 'h')]
    qc.rz(-2 * gamma * -0.125, 7)  # from: [('I', 'e'), ('I', 'f'), ('I', 'g'), ('Z', 'h')]
    qc.rz(-2 * gamma * -0.25, 6)  # from: [('I', 'e'), ('I', 'f'), ('Z', 'g'), ('I', 'h')]
    qc.rz(-2 * gamma * 0.125, 5)  # from: [('I', 'e'), ('Z', 'f'), ('I', 'g'), ('I', 'h')]
    qc.rz(-2 * gamma * 0.125, 5)  # from: [('I', 'e'), ('Z', 'f'), ('I', 'g'), ('Z', 'h')]
    qc.rz(-2 * gamma * 0.125, 7)  # from: [('I', 'e'), ('Z', 'f'), ('I', 'g'), ('Z', 'h')]
    qc.rz(-2 * gamma * 0.25, 0)  # from: [('Z', 'a'), ('I', 'b'), ('I', 'c'), ('I', 'd')]
    qc.rz(-2 * gamma * 0.25, 0)  # from: [('Z', 'a'), ('I', 'b'), ('I', 'e'), ('I', 'f')]
    qc.rz(-2 * gamma * 0.25, 0)  # from: [('Z', 'a'), ('I', 'b'), ('I', 'g'), ('I', 'h')]
    qc.rz(-2 * gamma * 0.125, 0)  # from: [('Z', 'a'), ('I', 'b'), ('Z', 'c'), ('I', 'd')]
    qc.rz(-2 * gamma * 0.125, 2)  # from: [('Z', 'a'), ('I', 'b'), ('Z', 'c'), ('I', 'd')]
    qc.rz(-2 * gamma * -0.125, 0)  # from: [('Z', 'a'), ('I', 'b'), ('Z', 'c'), ('Z', 'd')]
    qc.rzz(-2 * gamma * -0.125, 2, 3)  # from: [('Z', 'a'), ('I', 'b'), ('Z', 'c'), ('Z', 'd')]
    qc.rz(-2 * gamma * 0.125, 0)  # from: [('Z', 'a'), ('I', 'b'), ('Z', 'e'), ('I', 'f')]
    qc.rz(-2 * gamma * 0.125, 4)  # from: [('Z', 'a'), ('I', 'b'), ('Z', 'e'), ('I', 'f')]
    qc.rz(-2 * gamma * -0.125, 0)  # from: [('Z', 'a'), ('I', 'b'), ('Z', 'e'), ('Z', 'f')]
    qc.rzz(-2 * gamma * -0.125, 4, 5)  # from: [('Z', 'a'), ('I', 'b'), ('Z', 'e'), ('Z', 'f')]
    qc.rz(-2 * gamma * 0.125, 0)  # from: [('Z', 'a'), ('I', 'b'), ('Z', 'g'), ('I', 'h')]
    qc.rz(-2 * gamma * 0.125, 6)  # from: [('Z', 'a'), ('I', 'b'), ('Z', 'g'), ('I', 'h')]
    qc.rz(-2 * gamma * -0.125, 0)  # from: [('Z', 'a'), ('I', 'b'), ('Z', 'g'), ('Z', 'h')]
    qc.rzz(-2 * gamma * -0.125, 6, 7)  # from: [('Z', 'a'), ('I', 'b'), ('Z', 'g'), ('Z', 'h')]
    qc.cx(0, 1)
    qc.rzz(-2 * gamma * 0.125, 1, 2)  # from: [('Z', 'a'), ('Z', 'b'), ('Z', 'c'), ('I', 'd')]
    qc.cx(0, 1)
    qc.cx(0, 1)
    qc.cx(1, 2)
    qc.rzz(-2 * gamma * 0.125, 2, 3)  # from: [('Z', 'a'), ('Z', 'b'), ('Z', 'c'), ('Z', 'd')]
    qc.cx(1, 2)
    qc.cx(0, 1)
    qc.cx(0, 1)
    qc.rzz(-2 * gamma * 0.125, 1, 4)  # from: [('Z', 'a'), ('Z', 'b'), ('Z', 'e'), ('I', 'f')]
    qc.cx(0, 1)
    qc.cx(0, 1)
    qc.cx(1, 4)
    qc.rzz(-2 * gamma * 0.125, 4, 5)  # from: [('Z', 'a'), ('Z', 'b'), ('Z', 'e'), ('Z', 'f')]
    qc.cx(1, 4)
    qc.cx(0, 1)
    qc.cx(0, 1)
    qc.rzz(-2 * gamma * 0.125, 1, 6)  # from: [('Z', 'a'), ('Z', 'b'), ('Z', 'g'), ('I', 'h')]
    qc.cx(0, 1)
    qc.cx(0, 1)
    qc.cx(1, 6)
    qc.rzz(-2 * gamma * 0.125, 6, 7)  # from: [('Z', 'a'), ('Z', 'b'), ('Z', 'g'), ('Z', 'h')]
    qc.cx(1, 6)
    qc.cx(0, 1)
    qc.rz(-2 * gamma * -0.25, 2)  # from: [('Z', 'c'), ('I', 'd'), ('I', 'e'), ('I', 'f')]
    qc.rz(-2 * gamma * 0.25, 2)  # from: [('Z', 'c'), ('I', 'd'), ('I', 'g'), ('I', 'h')]
    qc.rz(-2 * gamma * 0.125, 2)  # from: [('Z', 'c'), ('I', 'd'), ('Z', 'e'), ('I', 'f')]
    qc.rz(-2 * gamma * 0.125, 4)  # from: [('Z', 'c'), ('I', 'd'), ('Z', 'e'), ('I', 'f')]
    qc.rz(-2 * gamma * 0.125, 2)  # from: [('Z', 'c'), ('I', 'd'), ('Z', 'e'), ('Z', 'f')]
    qc.rzz(-2 * gamma * 0.125, 4, 5)  # from: [('Z', 'c'), ('I', 'd'), ('Z', 'e'), ('Z', 'f')]
    qc.rz(-2 * gamma * 0.125, 2)  # from: [('Z', 'c'), ('I', 'd'), ('Z', 'g'), ('I', 'h')]
    qc.rz(-2 * gamma * 0.125, 6)  # from: [('Z', 'c'), ('I', 'd'), ('Z', 'g'), ('I', 'h')]
    qc.rz(-2 * gamma * -0.125, 2)  # from: [('Z', 'c'), ('I', 'd'), ('Z', 'g'), ('Z', 'h')]
    qc.rzz(-2 * gamma * -0.125, 6, 7)  # from: [('Z', 'c'), ('I', 'd'), ('Z', 'g'), ('Z', 'h')]
    qc.cx(2, 3)
    qc.rzz(-2 * gamma * -0.125, 3, 4)  # from: [('Z', 'c'), ('Z', 'd'), ('Z', 'e'), ('I', 'f')]
    qc.cx(2, 3)
    qc.cx(2, 3)
    qc.cx(3, 4)
    qc.rzz(-2 * gamma * 0.125, 4, 5)  # from: [('Z', 'c'), ('Z', 'd'), ('Z', 'e'), ('Z', 'f')]
    qc.cx(3, 4)
    qc.cx(2, 3)
    qc.cx(2, 3)
    qc.rzz(-2 * gamma * 0.125, 3, 6)  # from: [('Z', 'c'), ('Z', 'd'), ('Z', 'g'), ('I', 'h')]
    qc.cx(2, 3)
    qc.cx(2, 3)
    qc.cx(3, 6)
    qc.rzz(-2 * gamma * 0.125, 6, 7)  # from: [('Z', 'c'), ('Z', 'd'), ('Z', 'g'), ('Z', 'h')]
    qc.cx(3, 6)
    qc.cx(2, 3)
    qc.rz(-2 * gamma * 0.25, 4)  # from: [('Z', 'e'), ('I', 'f'), ('I', 'g'), ('I', 'h')]
    qc.rz(-2 * gamma * 0.125, 4)  # from: [('Z', 'e'), ('I', 'f'), ('Z', 'g'), ('I', 'h')]
    qc.rz(-2 * gamma * 0.125, 6)  # from: [('Z', 'e'), ('I', 'f'), ('Z', 'g'), ('I', 'h')]
    qc.rz(-2 * gamma * -0.125, 4)  # from: [('Z', 'e'), ('I', 'f'), ('Z', 'g'), ('Z', 'h')]
    qc.rzz(-2 * gamma * -0.125, 6, 7)  # from: [('Z', 'e'), ('I', 'f'), ('Z', 'g'), ('Z', 'h')]
    qc.cx(4, 5)
    qc.rzz(-2 * gamma * 0.125, 5, 6)  # from: [('Z', 'e'), ('Z', 'f'), ('Z', 'g'), ('I', 'h')]
    qc.cx(4, 5)
    qc.cx(4, 5)
    qc.cx(5, 6)
    qc.rzz(-2 * gamma * 0.125, 6, 7)  # from: [('Z', 'e'), ('Z', 'f'), ('Z', 'g'), ('Z', 'h')]
    qc.cx(5, 6)
    qc.cx(4, 5)

    for i in range(n_qubits):
        qc.rx(2 * beta, i)

qc.measure(range(n_qubits), range(n_qubits))

simulator = BasicSimulator()

compiled_circuit = transpile(qc, simulator)
job = simulator.run(compiled_circuit, shots=1024)
result = job.result()
counts = result.get_counts()

print(result.get_counts())
