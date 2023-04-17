import pandas as pd
import matplotlib.pyplot as plt

if __name__ == '__main__':
    # Enter performance data here 
    df = pd.DataFrame(
        {
            'Task size': [50, 100, 200],
            'Single thread': [14, 32, 53],
            '5 workers': [6, 11, 25],
            '10 workers': [4, 9, 21],
            '16 workers': [3, 7, 19],
            'Shared memory parallel': [4, 8, 10]
        }
    )


    df = df.set_index('Task size')

    label_dict = {
        '5 workers': '5 workers (ours)',
        '10 workers': '10 workers (ours)',
        '16 workers': '16 workers (ours)'
    }

    for col in ['5 workers', '10 workers', '16 workers', 'Single thread', 'Shared memory parallel']:
        plt.scatter([50,100,200], df[col], s=10)
        plt.plot(df[col], label=label_dict[col] if col in label_dict else col)
    plt.xticks([50,100,150,200])
    plt.ylim(0, 60)
    plt.xlabel('Task Size')
    plt.ylabel('Time')
    plt.legend()
    plt.savefig('Hoo-Ray performance.png')
    plt.show()