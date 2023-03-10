import os
import sys
import math

CLASS = 'prose_custom'
EPS = 1.0e-3

if __name__ == "__main__":

    # CLASS = A
    if CLASS == 'A':
        zeta_verify_value = 17.130235054029
    elif CLASS == 'B':
        zeta_verify_value = 22.712745482630986
    elif CLASS == 'prose_custom':
        zeta_verify_value = 22.6774060105612

    with open(os.path.join(sys.argv[1], "stdout.txt"), "r") as f:
        lines = f.readlines()
        zeta = float(lines[0].split(":")[1].strip())
        time = float(lines[1].split(":")[1].strip())
        err = abs( zeta - zeta_verify_value ) / zeta_verify_value
        if math.isnan(err):
            time = "error"
        elif err > EPS:
            time = -1 * time
    
    print(time)
