#include <stdio.h>

int main()
{
    int storedCoins = 0;
    while (1)
    {
        printf("Enter an input (10, 5, 0):");
        int number;
        scanf("%d", &number);
        if(number != 0 && number != 5 && number != 10)
        {
            printf("Invalid input\n");
            continue;
        }

        if (storedCoins + number >= 10)
        {
            printf("Output: Bottle");
            if (storedCoins + number == 15) printf(", Return");
            
            storedCoins = 0;
        }
        else
        {
            storedCoins += number;
            printf("Output: Nothing");
        }
        printf("\n");
    }
}