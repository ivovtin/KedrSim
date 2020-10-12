# ����� ������� ������������� ��������� ����

��� ��������� ���� ���������� ��������� ��������� �������:
```
git clone https://github.com/ivovtin/KedrSim
```

��� ������ ����� ��������� � ������� ����������:
```
make
```

������ �������������:
```
~/bin/ks < mc.cards > /dev/null
```

��������� ����������� ������ (����� - ������ �.�.)  <br />
GENE 53 0.01 5. 0. 85. -32.5 32.5 12.5              <br />

������ ������������� �� �������������� ��������:
```
qsub batch_test_atcmc.sh
```

������ � KDisplay ��������� �������������:                    <br />
```
KDisplay < simout/sim000001.dat -r -R19697
```

������ � KDisplay ��������� ����������������� �������        <br />
```
bzcat /space/data2/KEDR_RUNS/runs/daq019697.nat.bz2 | KDisplay -r -d3
```

