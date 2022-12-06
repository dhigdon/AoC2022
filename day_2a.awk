/A X/ || /B Y/ || /C Z/ {score += 3}
/A Y/ || /B Z/ || /C X/ {score += 6}
                        {score += index("XYZ",$2)}
END                     {print score}
